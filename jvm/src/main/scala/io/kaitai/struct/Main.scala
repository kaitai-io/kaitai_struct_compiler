package io.kaitai.struct

import java.io.File

import io.kaitai.struct.format.{ClassSpec, KSVersion, YAMLParseException}
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.components.LanguageCompilerStatic

object Main {
  KSVersion.current = BuildInfo.version

  case class CLIConfig(
    srcFiles: Seq[File] = Seq(),
    outDir: File = new File("."),
    targets: Seq[String] = Seq(),
    runtime: RuntimeConfig = RuntimeConfig()
  )

  val ALL_LANGS = LanguageCompilerStatic.NAME_TO_CLASS.keySet
  val VALID_LANGS = ALL_LANGS + "all"

  def parseCommandLine(args: Array[String]): Option[CLIConfig] = {
    val parser = new scopt.OptionParser[CLIConfig](BuildInfo.name) {
      override def showUsageOnError = true

      head(BuildInfo.name, BuildInfo.version)

      arg[File]("<file>...") unbounded() action { (x, c) =>
        c.copy(srcFiles = c.srcFiles :+ x) } text("source files (.ksy)")

      //      opt[File]('o', "outfile") valueName("<file>") action { (x, c) =>
      //        c.copy(outDir = x)
      //      } text("output filename (only if processing 1 file)")

      opt[String]('t', "target") required() unbounded() valueName("<language>") action { (x, c) =>
        // TODO: make support for something like "-t java,python"
        if (x == "all") {
          c.copy(targets = ALL_LANGS.toSeq)
        } else {
          c.copy(targets = c.targets :+ x)
        }
      } text(s"target languages (${VALID_LANGS.mkString(", ")})") validate { x =>
        if (VALID_LANGS.contains(x)) {
          success
        } else {
          failure(s"'${x}' is not a valid target language; valid ones are: ${VALID_LANGS.mkString(", ")}")
        }
      }

      opt[File]('d', "outdir") valueName("<directory>") action { (x, c) =>
        c.copy(outDir = x)
      } text("output directory (filenames will be auto-generated)")

      opt[String]("java-package") valueName("<package>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(javaPackage = x))
      } text("Java package (Java only, default: root package)")

      opt[String]("dotnet-namespace") valueName("<namespace>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(dotNetNamespace = x))
      } text(".NET Namespace (.NET only, default: Kaitai)")

      opt[String]("php-namespace") valueName("<namespace>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(phpNamespace = x))
      } text("PHP Namespace (PHP only, default: root package)")

      opt[Unit]("verbose") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(verbose = true))
      } text("verbose output")
      opt[Unit]("debug") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(debug = true))
      } text("enable debugging helpers (mostly used by visualization tools)")
      help("help") text("display this help and exit")
      version("version") text("output version information and exit")
    }

    parser.parse(args, CLIConfig())
  }

  def compileOne(srcFile: String, lang: String, outDir: String, config: RuntimeConfig): Unit = {
    if (config.verbose)
      Console.println(s"compiling ${srcFile} for ${lang}...")

    val spec = JavaKSYParser.localFileToSpec(srcFile)
    compileOne(spec, lang, outDir, config)
  }

  def compileOne(topClass: ClassSpec, lang: String, outDir: String, config: RuntimeConfig): Unit = {
    ClassCompiler.fromClassSpecToFile(topClass, LanguageCompilerStatic.byString(lang), outDir, config).compile
  }

  def compileAll(srcFile: String, config: CLIConfig): Unit = {
    if (config.runtime.verbose)
      Console.println(s"reading $srcFile...")

    val topClass = JavaKSYParser.localFileToSpec(srcFile)

    config.targets.foreach { lang =>
      try {
        if (config.runtime.verbose)
          Console.print(s"... compiling it for $lang... ")
        compileOne(topClass, lang, s"${config.outDir}/$lang", config.runtime)
      } catch {
        case e: Exception =>
          e.printStackTrace()
        case e: Error =>
          e.printStackTrace()
      }
    }
  }

  def main(args : Array[String]): Unit = {
    parseCommandLine(args)  match {
      case None => System.exit(1)
      case Some(config) =>
        config.srcFiles.foreach { srcFile =>
          try {
            config.targets match {
              case Seq(lang) =>
                // single target, just use target directory as is
                compileOne(srcFile.toString, lang, config.outDir.toString, config.runtime)
              case _ =>
                // multiple targets, use additional directories
                compileAll(srcFile.toString, config)
            }
          } catch {
            case e: YAMLParseException =>
              Console.println(e.getMessage)
          }
        }
    }
  }
}
