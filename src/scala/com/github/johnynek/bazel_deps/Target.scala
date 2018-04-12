package com.github.johnynek.bazel_deps

import com.github.johnynek.bazel_deps.IO.Path
import java.net.URI
import org.typelevel.paiges.Doc

object Target {
  def renderList[T](front: Doc, l: List[T], back: Doc, indent: Int = 4)(show: T => Doc): Doc =
    if (l.isEmpty) {
      Doc.empty
    } else {
      val spreadParts = Doc.intercalate(Doc.comma + Doc.line, l.map(show))
      spreadParts.bracketBy(front, back, indent)
    }

  def quote(s: String): Doc =
    Doc.text("\"%s\"".format(s))

  def fqnToLabelFragment(fqn: String): String =
    fqn.toLowerCase.replaceAll("[^a-z0-9]", "_")

  sealed abstract class Kind(override val toString: String)
  case object Library extends Kind("library")
  case object Import extends Kind("import")
  case object Test extends Kind("test")
  case object Binary extends Kind("binary")

  sealed abstract class SourceList {
    def render: Doc

  }
  object SourceList {
    case object Empty extends SourceList {
      def render: Doc = Doc.empty
    }

    case class Explicit(srcs: Set[String]) extends SourceList {
      def render: Doc =
        if (srcs.isEmpty) Doc.empty
        else {
          renderList(Doc.text("["), srcs.toList.sorted, Doc.text("]"))(quote)
            .grouped
        }
    }
    case class Globs(globs: List[String]) extends SourceList {
      def render: Doc =
        if (globs.isEmpty) Doc.empty
        else {
          val gstr = renderList(Doc.text("["), globs, Doc.text("]"))(quote)
            .grouped
          Doc.text("glob(") + gstr + Doc.text(")")
        }
    }
  }
}

case class Target(
  lang: Language,
  name: String,
  licenses: Set[String],
  jarUrls: Set[URI],
  jarSha256: Sha256Value,
  srcjarUrls: Option[Set[URI]] = None,
  srcjarSha256: Option[Sha256Value] = None,
  deps: Option[Set[Label]] = None,
  runtimeDeps: Option[Set[Label]] = None,
  testOnly: Option[Boolean] = None,
  exports: Option[Set[Label]] = None,
  neverLink: Option[Boolean] = None,
  generatedRuleName: Option[String] = None,
  generatedLinkableRuleName: Option[String] = None,
  defaultVisbility: Option[String] = Some("//visibility:public"),
  extraBuildFileContent: Option[String] = None,
  processorClasses: Set[ProcessorClass] = Set.empty,
  comment: Option[String] = None,
  bindInfo: (Label, String)
) {

  def toDoc() = {
    import Target._
    /**
     * e.g.
     * scala_library(
     * name = "foo",
     * deps = [ ],
     * exports = [ ],
     * runtime_deps = [ ],
     * visibility = ["//visibility:public"]
     * )
     */

    def sortKeys(prefix: Doc, suffix: Doc,  assignmentOperator: String, items: List[(String, Doc)]): Doc = {
      implicit val ordDoc: Ordering[Doc] = Ordering.by { d: Doc => d.renderWideStream.mkString }
      val sorted = items.collect { case (s, d) if !(d.isEmpty) => (s, d) }.sorted

      renderList(prefix, sorted, suffix) { case (k, v) =>
        k +: s"${assignmentOperator} " +: v
      }
    }

    def labelList(ls: Set[Label]): Doc = {
      renderList(Doc.text("["), ls.toList.map(_.asStringFrom(Path(Nil))).sorted, Doc.text("]"))(quote)
    }

    def renderExportedPlugins(pcs: Set[ProcessorClass]): Doc = {
      if (pcs.isEmpty) {
        Doc.empty
      } else {
        val exportedPlugins = renderList(
          Doc.text("["),
          pcs.toList.map(pc => ":" + getPluginTargetName(pcs, pc)).sorted,
          Doc.text("]")
        )(quote)

        sortKeys(Doc.text("java_library("), Doc.text(")"), "=", List(
          "name" -> Doc.text(name),
          "exported_plugins" -> exportedPlugins
        ))
      }
    }

    def getPluginTargetName(pcs: Set[ProcessorClass], pc: ProcessorClass) = {
      if (pcs.size == 1) {
        s"${name}_plugin"
      } else {
        s"${name}_plugin_${fqnToLabelFragment(pc.asString)}"
      }
    }

    def renderPlugins(pcs: Set[ProcessorClass], exports: Set[Label], licenses: Set[String]): Doc = {
      if (pcs.isEmpty) {
        Doc.empty
      } else {
        processorClasses.toList.sortBy(_.asString).map(renderPlugin(pcs, _, exports, licenses)).reduce((d1, d2) => d1 + d2)
      }
    }

    def renderPlugin(pcs: Set[ProcessorClass], pc: ProcessorClass, exports: Set[Label], licenses: Set[String]): Doc = {
      val pluginName = getPluginTargetName(pcs, pc)
      sortKeys(
        Doc.text("java_plugin("),
        Doc.text(")"),
        pluginName, List(
          "deps" -> labelList(exports + Label(None, Path(Nil), ":jar")),
          "licenses" -> renderLicenses(licenses),
          "processor_class" -> quote(pc.asString)
        ) ++ renderVisibility()
      ) + Doc.line
    }

    def renderBoolean(boolean: Boolean): Doc = {
      val pythonBoolean = if (boolean) {
        "True"
      } else {
        "False"
      }

      quote(pythonBoolean)
    }

    def renderSha256(sha256Value: Sha256Value): Doc = {
      quote(sha256Value.toHex)
    }

    def renderUriList(uriList: Iterable[URI]): Doc = {
      renderList(Doc.text("["), uriList.toList.map(_.toString), Doc.text("]"))(quote)
    }

    def renderVisibility(): Option[(String, Doc)] = {
      defaultVisbility.map { defaultVisbility =>
        "default_visibility" -> renderList(Doc.text("["), List(defaultVisbility), Doc.text("]"))(quote)
      }
    }

    def renderLicenses(licenses: Set[String]): Doc = {
      if (licenses.nonEmpty) {
        renderList(Doc.text("["), licenses.toList, Doc.text("]"))(quote)
      } else {
        Doc.empty
      }
    }

    val importArgs = List(
      "name" -> quote(name),
      // TODO: Can we get a version of this rule that doesn't require a license to be set?
      "licenses" -> renderLicenses(if (licenses.nonEmpty) licenses else Set("notice")),
      "jar_urls" -> renderUriList(jarUrls),
      "jar_sha256" -> renderSha256(jarSha256)
    ) ++
      srcjarUrls.map("srcjar_urls" -> renderUriList(_)) ++
      srcjarSha256.map("srcjar_sha256" -> renderSha256(_)) ++
      deps.map("deps" -> labelList(_)) ++
      runtimeDeps.map("runtime_deps" -> labelList(_)) ++
      testOnly.map("testonly_" -> renderBoolean(_)) ++
      exports.map("exports" -> labelList(_)) ++
      neverLink.map("neverlink" -> renderBoolean(_)) ++
      generatedRuleName.map("generated_rule_name" -> quote(_)) ++
      generatedLinkableRuleName.map("generated_linkable_rule_name" -> quote(_)) ++
      renderVisibility() ++
      exports.map("extra_build_file_content" -> renderPlugins(processorClasses, _, licenses).+(renderExportedPlugins(processorClasses)))

    val (label, bindName) = bindInfo
    val bindArgs = List(
      "actual" -> quote(label.asStringFrom(Path(Nil))),
      "name" -> quote(bindName)
    )

    def renderJson(items: List[(String, Doc)]): Doc = {
      val itemsWithQuotedKeys = items.map { case (key, value) =>
        s""""${key}"""" -> value
      }
      sortKeys(Doc.text("{"), Doc.text("}"), ":", itemsWithQuotedKeys)
    }

    // TODO: Break into lang, java_import_external args, and bind args
    val options = List(
      "lang" -> quote(lang.asString),
      "import_args" -> renderJson(importArgs),
      "bind_args" -> renderJson(bindArgs)
    )

    Doc.text(comment.getOrElse("")) + renderJson(options)
  }
}
