package com.github.johnynek.bazel_deps

import java.net.URI
import IO.{Path, Result}
import cats.Traverse
import cats.implicits._
import scala.util.{ Failure, Success, Try }
import org.typelevel.paiges.Doc

object Writer {

  /**
   * Takes a BUILD file path and generated contents, and returns the formatted version of those contents (e.g. with
   * buildifier).
   */
  type BuildFileFormatter = ((IO.Path, String) => String)
//
//  private val buildFileName = "BUILD"
//
//  private def buildFileContents(buildFilePath: IO.Path, buildHeader: String, ts: List[Target], formatter: BuildFileFormatter): String = {
//    def withNewline(s: String): String =
//      if (s.isEmpty) ""
//      else s + "\n"
//
//    formatter(buildFilePath, ts.sortBy(_.name)
//      .map(_.toDoc().render(60))
//      .mkString(withNewline(buildHeader), "\n\n", "\n"))
//  }
//
//  def createBuildFiles(buildHeader: String, ts: List[Target], formatter: BuildFileFormatter): Result[Int] = {
//    val pathGroups = ts.groupBy(_.name).toList
//
//    Traverse[List].traverseU(pathGroups) {
//      case (filePath, ts) =>
//        def data(bf: IO.Path) = buildFileContents(bf, buildHeader, ts, formatter)
//        for {
//          b <- IO.exists(filePath)
//          _ <- if (b) IO.const(false) else IO.mkdirs(filePath)
//          bf = filePath.child(buildFileName)
//          _ <- IO.writeUtf8(bf, data(bf))
//        } yield ()
//    }
//      .map(_.size)
//  }
//
//  def compareBuildFiles(buildHeader: String, ts: List[Target], formatter: BuildFileFormatter): Result[List[IO.FileComparison]] = {
//    val pathGroups = ts.groupBy(_.name).toList
//
//    Traverse[List].traverseU(pathGroups) {
//      case (filePath, ts) =>
//        def data(bf: IO.Path) = buildFileContents(bf, buildHeader, ts, formatter)
//        val bf = filePath.child(buildFileName)
//        IO.compare(bf, data(bf))
//    }
//  }

  def workspace(
    depsFile: String,
    targets: List[Target]
  ): String = {
    val renderedTargets = Doc.intercalate(Doc.comma + Doc.line, targets.map(_.toDoc))
      .bracketBy(Doc.text("["), Doc.space.repeat(4) + Doc.text("]"), 8)
      .render(80)

    s"""# Do not edit. bazel-deps autogenerates this file from ${depsFile}.
      |def list_dependencies():
      |    return $renderedTargets
    """.stripMargin
  }

  def language(g: Graph[MavenCoordinate, Unit],
    model: Model): UnversionedCoordinate => Language = {
    /**
     * Here are all the explicit artifacts
     */
    val uvToVerExplicit = g.nodes.map { c => (c.unversioned, c) }.toMap

    val langCache = scala.collection.mutable.Map[UnversionedCoordinate, Language]()
    def lang(u: UnversionedCoordinate): Language = langCache.getOrElseUpdate(u, {
      import Language.{ Java, Scala }

      model.dependencies.languageOf(u) match {
        case Some(l) => l
        case None =>
          Label.replaced(u, model.getReplacements) match {
            case Some((_, l)) => l
            case None =>
              // if you have any scala dependencies, you have to be handled by the
              // scala rule for now, otherwise we say java
              g.hasSource(uvToVerExplicit(u))
                .iterator
                .map(_.destination)
                .map { c => lang(c.unversioned) }
                .collectFirst {
                  case s@Scala(v, _) =>
                    val mangled = s.removeSuffix(u.asString).isDefined
                    Scala(v, mangled)
                }
                .getOrElse(Java)
          }
      }
    })

    { m => lang(m) }
  }

  def targets(
    g: Graph[MavenCoordinate, Unit],
    model: Model,
    duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
    jarInfo: Map[MavenCoordinate, Try[JarInfo]],
    srcjarInfo: Map[MavenCoordinate, Try[JarInfo]]
  ): Either[List[UnversionedCoordinate], List[Target]] = {
    /**
     * Check that all the exports are well-defined
     * TODO make sure to write targets for replaced nodes
     */
    val badExports =
      g.nodes.filter { c =>
        model.dependencies.exportedUnversioned(c.unversioned, model.getReplacements).isLeft
      }

    /**
     * Here are all the explicit artifacts
     */
    val uvToVerExplicit = g.nodes.map { c => (c.unversioned, c) }.toMap
    /**
     * Here are any that are replaced, they may not appear above:
     */
    val uvToRep = model.getReplacements.unversionedToReplacementRecord

    /**
     * Here are all the unversioned artifacts we need to create targets for:
     */
    val allUnversioned: Set[UnversionedCoordinate] = uvToVerExplicit.keySet.union(uvToRep.keySet)


    if (badExports.nonEmpty) {
      Left(badExports.toList.map(_.unversioned))
    } else {
      val rootName = model.getOptions.getThirdPartyDirectory
      val licenses = model.getOptions.getLicenses
      val pathInRoot = rootName.parts

      val nodes = g.nodes
      val servers = model.getOptions.getResolvers.map(s => (s.id, s.url)).toMap
      val langFn = language(g, model)
      val prefix = model.getOptions.getNamePrefix

      val cache = scala.collection.mutable.Map[MavenCoordinate, Target]()


      def unversionedCoordinateToTargetName(unversionedCoordinate: UnversionedCoordinate): Label = {
        Label.localTarget(pathInRoot, unversionedCoordinate, langFn(unversionedCoordinate))
      }

      /*
       * We make 1 label for each target, the path
       * and name are derived from the MavenCoordinate
       */
      def coordToTarget(coord: MavenCoordinate): Target = cache.getOrElseUpdate(coord, {
        val group = coord.group
        val artifactId = coord.artifact
        val version = coord.version
        val unversionedCoord = coord.unversioned
        val isRoot = model.dependencies.roots(coord)
        val comment = duplicates.get(coord.unversioned) match {
          case Some(vs) =>
            val status =
              if (isRoot) s"fixed to ${version.asString}"
              else if (vs.map(_.destination.version).max == version) s"promoted to ${version.asString}"
              else s"downgraded to ${version.asString}"

            s"""# duplicates in ${coord.unversioned.asString} $status\n""" +
              vs.filterNot(e => replaced(e.source)).map { e =>
                s"""# - ${e.source.asString} wanted version ${e.destination.version.asString}\n"""
              }.toSeq.sorted.mkString("")
          case None =>
            ""
        }

        def unversionedCoordtoLabel(unversionedCoord: UnversionedCoordinate, namePrefix: NamePrefix): Label = {
          Label(Some(unversionedCoord.toBazelRepoName(namePrefix)), Path(Nil), "")
        }

        val coordinateDeps = g.hasSource(coord)
        val depLabels = coordinateDeps.map { edge =>
          val unversionedDestination = edge.destination.unversioned
          replacedLabel(unversionedDestination).getOrElse(unversionedCoordtoLabel(unversionedDestination, prefix))
        }
        val (label, lang) = {
          Label.replaced(unversionedCoord, model.getReplacements)
            .getOrElse {
              (Label.parse(unversionedCoord.bindTarget(model.getOptions.getNamePrefix)), langFn(unversionedCoord))
            }
        }

        // Build explicit exports, no need to add these to runtime deps
        val uvexports = model.dependencies
          .exportedUnversioned(unversionedCoord, model.getReplacements)
          .right
          .get
          .map(u => replacedLabel(u).getOrElse(unversionedCoordtoLabel(u, prefix)))
          .toSet

        val (exports, runtimeDeps, deps) = model.getOptions.getTransitivity match {
          case Transitivity.Exports => (Some(depLabels), None, None)
          case Transitivity.RuntimeDeps => (None, Some(depLabels), None)
          case Transitivity.Deps => (None, None, Some(depLabels))
        }

        val currentJarInfo = jarInfo(coord).recover {
          case e => throw new Exception(s"Failed to get jar information for ${coord}. Error: ${e}")
        }.get

        val currentSrcJarInfo = srcjarInfo(coord).toOption

        Target(
          lang = lang,
          name = unversionedCoord.toBazelRepoName(prefix),
          licenses = licenses,
          jarUrls = currentJarInfo.jarUrls,
          jarSha256 = currentJarInfo.sha256Value,
          srcjarUrls = currentSrcJarInfo.map(_.jarUrls),
          srcjarSha256 = currentSrcJarInfo.map(_.sha256Value),
          exports = exports.map(_ ++ uvexports),
          runtimeDeps = runtimeDeps.map(_ -- uvexports),
          deps = deps.map(_ -- uvexports),
          processorClasses = getProcessorClasses(unversionedCoord),
          comment = Some(comment),
          bindInfo = (Label.externalJar(lang, unversionedCoord, prefix), coord.unversioned.toBindingName(prefix))
        )
      })

      def replacedLabel(u: UnversionedCoordinate): Option[Label] = {
        Label.replaced(u, model.getReplacements).map { case (lab, lang) =>
          lab
        }
      }

      def targetFor(coord: MavenCoordinate): Target = coordToTarget(coord)

      def getProcessorClasses(unversionedCoord: UnversionedCoordinate): Set[ProcessorClass] =
        (for {
          m <- model.dependencies.toMap.get(unversionedCoord.group)
          projectRecord <- m.get(ArtifactOrProject(unversionedCoord.artifact.asString))
        } yield projectRecord.processorClasses).flatten.getOrElse(Set.empty)

      def replaced(m: MavenCoordinate): Boolean = model.getReplacements.get(m.unversioned).isDefined

      Right(nodes.filterNot(replaced).toList.sortBy(_.asString).map(targetFor))
    }
  }
}
