package io.kaitai.struct

import java.io.File

import io.kaitai.struct.format.ClassSpec
import io.kaitai.struct.languages._

object Main {
  class Config(
     val verbose: Boolean = false,
     val javaPackage: String = ""
  )

  case class CLIConfig(
    srcFiles: Seq[File] = Seq(),
    outDir: File = new File("."),
    targets: Seq[String] = Seq(),

    private val _verbose: Boolean = false,
    private val _javaPackage: String = ""
  ) extends Config(_verbose, _javaPackage)

  val ALL_LANGS = Set("cpp_stl", "java", "javascript", "python", "ruby")
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
        c.copy(_javaPackage = x)
      } text(s"Java package (Java only, default: root package)")

      opt[Unit]("verbose") action { (x, c) =>
        c.copy(_verbose = true)
      } text(s"verbose output")
      help("help") text("display this help and exit")
      version("version") text("output version information and exit")
    }

    parser.parse(args, CLIConfig())
  }

  def compileOne(srcFile: String, lang: String, outDir: String, config: Config): Unit = {
    if (config.verbose)
      Console.println(s"compiling ${srcFile} for ${lang}...")

    ClassCompiler.fromLocalFileToFile(srcFile, LanguageCompilerStatic.byString(lang), outDir, config).compile
  }

  def compileOne(topClass: ClassSpec, lang: String, outDir: String, config: Config): Unit = {
    ClassCompiler.fromClassSpecToFile(topClass, LanguageCompilerStatic.byString(lang), outDir, config).compile
  }

  def compileAll(srcFile: String, config: CLIConfig): Unit = {
    if (config.verbose)
      Console.println(s"reading ${srcFile}...")

    val topClass = ClassCompiler.localFileToSpec(srcFile.toString)

    config.targets.foreach { lang =>
      try {
        if (config.verbose)
          Console.print(s"... compiling it for ${lang}... ")
        compileOne(topClass, lang, s"${config.outDir}/$lang", config)
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
          config.targets match {
            case Seq(lang) =>
              // single target, just use target directory as is
              compileOne(srcFile.toString, lang, config.outDir.toString, config)
            case _ =>
              // multiple targets, use additional directories
              compileAll(srcFile.toString, config)
          }
        }
    }
  }
}
