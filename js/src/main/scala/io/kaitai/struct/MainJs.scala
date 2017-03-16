package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, JavaScriptClassSpecs, JavaScriptKSYParser, KSVersion}
import io.kaitai.struct.languages.components.LanguageCompilerStatic

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExport
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@JSExport
object MainJs {
  KSVersion.current = BuildInfo.version

  @JSExport
  def compile(langStr: String, yaml: js.Object, importer: JavaScriptImporter, debug: Boolean = false): js.Promise[js.Dictionary[String]] = {
    try {
      val config = new RuntimeConfig(debug = debug)
      val lang = LanguageCompilerStatic.byString(langStr)

      val yamlScala = JavaScriptKSYParser.yamlJavascriptToScala(yaml)
      val firstSpec = ClassSpec.fromYaml(yamlScala)
      val specs = new JavaScriptClassSpecs(importer, firstSpec)
      Main.importAndPrecompile(specs, config).map { (_) =>
        specs.flatMap({ case (_, spec) =>
          val files = Main.compile(spec, lang, config).files
          files.map((x) => x.fileName -> x.contents).toMap
        }).toJSDictionary
      }.toJSPromise
    } catch {
      case err: Throwable => Future { throw err }.toJSPromise
    }
  }

  @JSExport
  lazy val languages: js.Array[String] = LanguageCompilerStatic.NAME_TO_CLASS.keys.toSeq.sorted.toJSArray

  @JSExport
  lazy val version = BuildInfo.version

  @JSExport
  lazy val buildDate = BuildInfo.builtAtString
}
