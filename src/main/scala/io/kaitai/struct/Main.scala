package io.kaitai.struct

import java.io.File

import io.kaitai.struct.languages._

object Main {
  case class Config(
    srcFiles: Seq[File] = Seq(),
    outDir: File = new File("."),
    targets: Seq[String] = Seq(),
    verbose: Boolean = false,

    javaPackage: String = ""
  )

  val ALL_LANGS = Set("java", "javascript", "python", "ruby")
  val VALID_LANGS = ALL_LANGS + "all"

  def parseCommandLine(args: Array[String]): Option[Config] = {
    val parser = new scopt.OptionParser[Config](BuildInfo.name) {
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
        c.copy(javaPackage = x)
      } text(s"Java package (Java only, default: root package)")

      opt[Unit]("verbose") action { (x, c) =>
        c.copy(verbose = true)
      } text(s"verbose output")
      help("help") text("display this help and exit")
      version("version") text("output version information and exit")
    }

    parser.parse(args, Config())
  }

  def compile(srcFile: String, lang: String, outDir: String, config: Config): Unit = {
    try {
      if (config.verbose)
        Console.println(s"compiling ${srcFile} for ${lang}...")

      val lc = lang match {
        case "java" => new JavaCompiler(config.verbose, s"${outDir}/src/${config.javaPackage.replace('.', '/')}", config.javaPackage)
        case "javascript" => new JavaScriptCompiler(config.verbose, outDir)
        case "python" => new PythonCompiler(config.verbose, outDir)
        case "ruby" => new RubyCompiler(config.verbose, outDir)
      }

      new ClassCompiler(srcFile, lc).compile
    } catch {
      case e: Exception =>
        e.printStackTrace()
      case e: Error =>
        e.printStackTrace()
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
              compile(srcFile.toString, lang, config.outDir.toString, config)
            case _ =>
              // multiple targets, use additional directories
              config.targets.foreach { lang =>
                compile(srcFile.toString, lang, s"${config.outDir}/$lang", config)
              }
          }
        }
    }
  }
}
