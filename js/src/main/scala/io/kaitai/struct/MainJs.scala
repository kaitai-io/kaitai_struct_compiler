package io.kaitai.struct

import io.kaitai.struct.format.{JavaScriptKSYParser, KSVersion}
import io.kaitai.struct.languages.components.LanguageCompilerStatic

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("MainJs")
object MainJs {
  KSVersion.current = Version.version

  @JSExport
  def compile(langStr: String, yaml: js.Object, importer: JavaScriptImporter, debug: Boolean = false): js.Promise[js.Dictionary[String]] = {
    try {
      // TODO: add proper enabled by a flag
      //Log.initFromVerboseFlag(Seq("file", "value", "parent", "type_resolve", "type_valid", "seq_sizes", "import"))
      val config = new RuntimeConfig(autoRead = !debug, readStoresPos = debug)
      val lang = LanguageCompilerStatic.byString(langStr)

      JavaScriptKSYParser.yamlToSpecs(yaml, importer, config).map { (specs) =>
        specs.flatMap({ case (_, spec) =>
          val files = Main.compile(specs, spec, lang, config).files
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
  lazy val version = Version.version

  @JSExport
  lazy val buildDate = Version.gitTime
}
