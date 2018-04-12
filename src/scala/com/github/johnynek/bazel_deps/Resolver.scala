package com.github.johnynek.bazel_deps

import java.security.MessageDigest
import java.io.{File, FileInputStream}
import java.net.URI
import java.nio.file.Path
import java.util
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.collection.{CollectRequest, CollectResult}
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.graph.{Dependency, DependencyNode, DependencyVisitor, Exclusion}
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.repository._
import org.eclipse.aether.resolution.{ArtifactRequest, ArtifactResult}
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import org.eclipse.aether.internal.impl.Maven2RepositoryLayoutFactory
import org.eclipse.aether.internal.impl.EnhancedLocalRepositoryManagerFactory
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

case class ResolveFailure(message: String,
  m: MavenCoordinate,
  extension: String,
  failures: List[Exception]) extends Exception(message)

class Resolver(servers: List[MavenServer], resolverCachePath: Path) {

  private val system = {
    val locator = MavenRepositorySystemUtils.newServiceLocator
    locator.addService(classOf[RepositoryConnectorFactory], classOf[BasicRepositoryConnectorFactory])
    locator.addService(classOf[TransporterFactory], classOf[FileTransporterFactory])
    locator.addService(classOf[TransporterFactory], classOf[HttpTransporterFactory])

    locator.setErrorHandler(new DefaultServiceLocator.ErrorHandler {
      override def serviceCreationFailed(t: Class[_], impl: Class[_], exception: Throwable) {
        exception.printStackTrace()
      }
    })

    locator.getService(classOf[RepositorySystem])
  }

  private val session = {
    val s = MavenRepositorySystemUtils.newSession()
    val localRepo = new LocalRepository(resolverCachePath.toString)
//    s.setLocalRepositoryManager(new EnhancedLocalRepositoryManagerFactory().newInstance(s, localRepo))
    s.setLocalRepositoryManager(system.newLocalRepositoryManager(s, localRepo))
    s
  }

  private val repositories =
    servers.map { case MavenServer(id, t, u) =>
      new RemoteRepository.Builder(id, t, u)
        // Disable warnings from bazel-deps not passing checksums to Aether.  Use the default update policy.
        .setPolicy(new RepositoryPolicy(true, RepositoryPolicy.UPDATE_POLICY_DAILY, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
        .build
    }.asJava

  private val idToRepository = {
    repositories.asScala.map { repository =>
      repository.getId -> repository
    }.toMap

  }
  private val localRepositoryId = session.getLocalRepository.getId

  private def getRemoteRepository(artifactResult: ArtifactResult): RemoteRepository = {
    val id = artifactResult.getRepository.getId

    if (id == localRepositoryId) {
      val localRequest = new LocalArtifactRequest(artifactResult.getArtifact, repositories, null)
      session.getLocalRepositoryManager.find(session, localRequest).getRepository
    } else {
      idToRepository(id)
    }
  }

  /**
   * Here is where the IO happens
   */
  private def request(m: MavenCoordinate, ml: Model): CollectResult = {
    val collectRequest = new CollectRequest()
    val ex = ml.dependencies.excludes(m.unversioned)
    val exclusions = new util.ArrayList[Exclusion]()
    for (elem <- ex){
      val exclusion = new Exclusion(elem.group.asString, elem.artifact.asString, "", "jar")
      exclusions.add(exclusion)
    }
    collectRequest.setRoot(new Dependency(new DefaultArtifact(m.asString), "", false, exclusions))
    collectRequest.setRepositories(repositories)
    system.collectDependencies(session, collectRequest);
  }

  /**
   * Get shas and urls for artifacts
   */
  def getCoordinateInfo(
    m: Iterable[MavenCoordinate],
    extension: String,
    classifier: Option[String] = None
  ): Map[MavenCoordinate, Try[JarInfo]] = {
    def toArtifactRequest(m: MavenCoordinate, extension: String, classifier: Option[String]): ArtifactRequest = {
      val context = null
      new ArtifactRequest(m.toArtifact(extension, classifier), repositories, context)
    }

    def liftKeys[K, V](
      mavenCoordinates: Iterable[K],
      tmap: Try[Map[K, Try[V]]]
    ): Map[K, Try[V]] = {
      mavenCoordinates.map { coord =>
        coord -> tmap.flatMap(_ (coord))
      }.toMap
    }

    def getExt(
      mavenCoordinates: Seq[MavenCoordinate],
      ext: String,
      classifier: Option[String]
    )(
      toSha: File => Try[Sha256Value]
    ): Map[MavenCoordinate, Try[JarInfo]] = {

      liftKeys(
        mavenCoordinates,
        Try {
          val artifactResults = mavenCoordinates.map { coordinate =>
            Try {
              system.resolveArtifact(
                session,
                toArtifactRequest(coordinate, ext, classifier)
              )
            }
          }

          mavenCoordinates.zip(artifactResults).collect { case (coord, Success(result)) =>
            val info = getFile(coord, ext, result)
              .flatMap { file =>
                toSha(file).map { sha256Value =>
                  val layoutFactory = new Maven2RepositoryLayoutFactory
                  val repository = getRemoteRepository(result)
                  val repositoryLayout = layoutFactory.newInstance(session, getRemoteRepository(result))
                  val artifactPath = repositoryLayout.getLocation(result.getArtifact, false)
                  val artifactURI = URI.create(repository.getUrl).resolve(artifactPath)

                  JarInfo(Set(artifactURI), sha256Value)
                }
              }

            coord -> info
          }.toMap
        }
      )
    }

    getExt(m.toList, extension, classifier)(computeShaOf)
  }

  private def getFile(m: MavenCoordinate, ext: String, a: ArtifactResult): Try[File] =
    a.getArtifact match {
      case null => Failure(ResolveFailure("null artifact", m, ext, a.getExceptions.asScala.toList))
      case art =>
        val f = art.getFile
        if (f == null) {
          Failure(ResolveFailure("null file", m, ext, a.getExceptions.asScala.toList))
        }
        else Success(f)
    }

  private def computeShaOf(f: File): Try[Sha256Value] = Try {
    val sha = MessageDigest.getInstance("SHA-256")
    val fis = new FileInputStream(f)
    try {
      var n = 0;
      val buffer = new Array[Byte](8192)
      while (n != -1) {
        n = fis.read(buffer)
        if (n > 0) sha.update(buffer, 0, n)
      }
      Success(Sha256Value(sha.digest.map("%02X".format(_)).mkString.toLowerCase))
    }
    catch {
      case NonFatal(err) => Failure(err)
    }
    finally {
      fis.close
    }
  }.flatten

  type Node = MavenCoordinate

  def addAll(deps: Graph[Node, Unit], coords: TraversableOnce[MavenCoordinate], m: Model): Graph[Node, Unit] =
    coords.foldLeft(deps)(addToGraph(_, _, m))

  def addToGraph(deps: Graph[Node, Unit], dep: MavenCoordinate, m: Model): Graph[Node, Unit] = {
    val visitor = new Visitor(deps, m)
    val result = request(dep, m).getRoot.accept(visitor)
    visitor.currentDeps
  }

  private class Visitor(initDeps: Graph[Node, Unit], model: Model) extends DependencyVisitor {
    var currentDeps = initDeps
    private var visited: Set[(Dependency, Boolean)] = Set.empty
    private var stack: List[Dependency] = Nil

    def coord(a: Dependency): MavenCoordinate = {
      val artifact = a.getArtifact
      MavenCoordinate(MavenGroup(artifact.getGroupId),
        MavenArtifactId(artifact.getArtifactId),
        Version(artifact.getVersion))
    }

    def addEdgeTo(d: Dependency): Boolean =
      (!d.isOptional) &&
      (d.getScope.toLowerCase match {
        case "" => true // default
        case "compile" => true // default
        case "provided" => false // TODO: we will need to revisit this
        case "runtime" => true // TODO: we should only add these to runtime deps
        case "test" => false
        case "system" => false // these should not be in maven, and should be handled by replacements
        case "import" =>
          // This means pull all the dependencies from a pom we are pointing to
          sys.error("unsupported")
        case other => sys.error(s"unknown scope: $other in $d")
      })

    /**
     * Some maven artifacts are replaced, meaning we deal with them and
     * their dependencies manually. If this is true, never follow (but
     * we do add the edges to the node in such cases
     */
    def notReplaced(m: MavenCoordinate): Boolean =
      model.getReplacements
        .get(m.unversioned)
        .isEmpty

    def excludeEdge(src: MavenCoordinate, dest: MavenCoordinate): Boolean =
      model.dependencies.excludes(src.unversioned).contains(dest.unversioned)

    def visitEnter(depNode: DependencyNode): Boolean = {
      val dep = depNode.getDependency
      val shouldAdd = addEdgeTo(dep)
      /**
       * unfollowed nodes are distinct from followed nodes.
       * If project a has an optional dependency on b, that does
       * not mean another project does not have a non-optional dependency
       */
      if (visited((dep, shouldAdd))) false
      else {
        visited = visited + (dep -> shouldAdd)
        val mvncoord = coord(dep)
        if (shouldAdd) {
          currentDeps = currentDeps.addNode(mvncoord)
        }
        else {
          //println(s"$dep, ${dep.isOptional}, ${dep.getScope}")
        }
        stack match {
          case Nil => ()
          case h :: _ =>
            val src = coord(h)
            if (shouldAdd && !excludeEdge(src, mvncoord)) {
              currentDeps = currentDeps
                .addEdge(Edge(src, mvncoord, ()))
            }
        }
        stack = dep :: stack
        shouldAdd && notReplaced(mvncoord)
      }
    }
    def visitLeave(dep: DependencyNode): Boolean = {
      require(stack.head == dep.getDependency, s"stack mismatch: ${stack.head} != ${dep.getDependency}")
      stack = stack.tail
      true
    }
  }
}
