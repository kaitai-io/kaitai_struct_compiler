package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, JavaScriptClassSpecs, JavaScriptKSYParser, KSVersion}
import io.kaitai.struct.languages.components.LanguageCompilerStatic

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExport

@JSExport
object MainJs {
  KSVersion.current = BuildInfo.version

  @JSExport
  def compile(langStr: String, yaml: js.Object, debug: Boolean = false): js.Array[String] = {
    val config = new RuntimeConfig(debug = debug)
    val lang = LanguageCompilerStatic.byString(langStr)

    val specs = new JavaScriptClassSpecs()
    val yamlScala = JavaScriptKSYParser.yamlJavascriptToScala(yaml)
    val firstSpec = ClassSpec.fromYaml(yamlScala)
    TypeProcessor.processTypesMany(specs, firstSpec)

    val res = specs.flatMap { case (specName, spec) =>
      val (out1, out2, cc) = ClassCompiler.fromClassSpecToString(spec, lang, config)
      cc.compile
      out2 match {
        case None => js.Array(out1.result)
        case Some(outHdr) => js.Array(out1.result, outHdr.result)
      }
    }

    res.toJSArray
  }

  @JSExport
  lazy val languages: js.Array[String] = LanguageCompilerStatic.NAME_TO_CLASS.keys.toSeq.sorted.toJSArray

  @JSExport
  lazy val version = BuildInfo.version

  @JSExport
  lazy val buildDate = BuildInfo.builtAtString
}
