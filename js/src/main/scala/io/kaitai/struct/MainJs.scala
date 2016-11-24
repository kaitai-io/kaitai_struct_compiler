package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, JavaScriptKSYParser}
import io.kaitai.struct.languages.components.LanguageCompilerStatic

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExport

@JSExport
object MainJs {
  @JSExport
  def compile(langStr: String, yaml: js.Object, debug: Boolean = false): js.Array[String] = {
    val config = new RuntimeConfig(verbose = true, debug = debug)
    val lang = LanguageCompilerStatic.byString(langStr)

    val yamlScala = JavaScriptKSYParser.yamlJavascriptToScala(yaml)
    val spec = ClassSpec.fromYaml(yamlScala)
    TypeProcessor.processTypes(spec)

    val (out1, out2, cc) = ClassCompiler.fromClassSpecToString(spec, lang, config)
    cc.compile

    out2 match {
      case None => js.Array(out1.result)
      case Some(outHdr) => js.Array(out1.result, outHdr.result)
    }
  }

  @JSExport
  lazy val languages: js.Array[String] = LanguageCompilerStatic.NAME_TO_CLASS.keys.toSeq.sorted.toJSArray

  @JSExport
  lazy val version = BuildInfo.version

  @JSExport
  lazy val buildDate = BuildInfo.builtAtString
}
