package io.kaitai.struct

import java.io.{File, FileWriter}

import io.kaitai.struct.CompileLog._
import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.{ClassSpec, ClassSpecs, KSVersion, YAMLParseException}
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.components.LanguageCompilerStatic

object JavaMain {
  KSVersion.current = BuildInfo.version

  case class CLIConfig(
    verbose: Seq[String] = Seq(),
    srcFiles: Seq[File] = Seq(),
    outDir: File = new File("."),
    targets: Seq[String] = Seq(),
    throwExceptions: Boolean = false,
    jsonOutput: Boolean = false,
    importPaths: Seq[String] = Seq(),
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

      val importPathExample = List("<directory>", "<directory>", "...").mkString(File.pathSeparator)
      opt[String]('I', "import-path") valueName(importPathExample) action { (x, c) =>
        c.copy(importPaths = c.importPaths ++ x.split(File.pathSeparatorChar))
      } text(".ksy library search path(s) for imports (see also KSPATH env variable)")

      opt[String]("java-package") valueName("<package>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(javaPackage = x))
      } text("Java package (Java only, default: root package)")

      opt[String]("dotnet-namespace") valueName("<namespace>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(dotNetNamespace = x))
      } text(".NET Namespace (.NET only, default: Kaitai)")

      opt[String]("php-namespace") valueName("<namespace>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(phpNamespace = x))
      } text("PHP Namespace (PHP only, default: root package)")

      opt[Boolean]("opaque-types") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(opaqueTypes = x))
      } text("opaque types allowed, default: false")

      opt[Unit]("ksc-exceptions") action { (x, c) =>
        c.copy(throwExceptions = true)
      } text("ksc throws exceptions instead of human-readable error messages")

      opt[Unit]("ksc-json-output") action { (x, c) =>
        c.copy(jsonOutput = true)
      } text("output compilation results as JSON to stdout")

      opt[String]("verbose") action { (x, c) =>
        // TODO: make support for something like "--verbose file,parent"
        if (x == "all") {
          c.copy(verbose = Log.VALID_SUBSYS)
        } else {
          c.copy(verbose = c.verbose :+ x)
        }
      } text("verbose output") validate { x =>
        if (x == "all" || Log.VALID_SUBSYS.contains(x)) {
          success
        } else {
          failure(s"'$x' is not a valid verbosity flag; valid ones are: ${Log.VALID_SUBSYS.mkString(", ")}")
        }
      }

      opt[Unit]("debug") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(debug = true))
      } text("enable debugging helpers (mostly used by visualization tools)")
      help("help") text("display this help and exit")
      version("version") text("output version information and exit")
    }

    parser.parse(args, CLIConfig())
  }

  private def envPaths: List[String] =
    sys.env.get("KSPATH").toList.flatMap((x) => x.split(File.pathSeparatorChar))

  def main(args: Array[String]): Unit = {
    parseCommandLine(args) match {
      case None => System.exit(1)
      case Some(config0) =>
        val config = config0.copy(importPaths = config0.importPaths ++ envPaths)
        new JavaMain(config).run()
    }
  }
}

class JavaMain(config: CLIConfig) {
  import JavaMain._

  Log.initFromVerboseFlag(config.verbose)

  def run(): Unit = {
    val logs: Map[String, InputEntry] = config.srcFiles.map { srcFile =>
      val log = if (config.throwExceptions) {
        compileOneInput(srcFile.toString)
      } else {
        try {
          compileOneInput(srcFile.toString)
        } catch {
          case ex: Throwable =>
            InputFailure(List(exceptionToCompileError(ex, srcFile.toString)))
        }
      }
      srcFile.toString -> log
    }.toMap
    if (config.jsonOutput)
      Console.println(JSON.mapToJson(logs))
  }

  private def compileOneInput(srcFile: String) = {
    Log.fileOps.info(() => s"parsing $srcFile...")
    val specs = JavaKSYParser.localFileToSpecs(srcFile, config)

    val output: Map[String, Map[String, SpecEntry]] = config.targets match {
      case Seq(lang) =>
        // single target, just use target directory as is
        val out = compileOneLang(specs, lang, config.outDir.toString)
        Map(lang -> out)
      case _ =>
        // multiple targets, use additional directories
        compileAllLangs(specs, config)
    }
    InputSuccess(
      specs.firstSpec.nameAsStr,
      output
    )
  }

  def compileAllLangs(specs: ClassSpecs, config: CLIConfig): Map[String, Map[String, SpecEntry]] = {
    config.targets.map { lang =>
      lang -> compileOneLang(specs, lang, s"${config.outDir}/$lang")
    }.toMap
  }

  def compileOneLang(specs: ClassSpecs, langStr: String, outDir: String): Map[String, SpecEntry] = {
    Log.fileOps.info(() => s"... compiling it for $langStr... ")
    val lang = LanguageCompilerStatic.byString(langStr)
    specs.map { case (_, classSpec) =>
      val res = try {
        compileSpecAndWriteToFile(classSpec, lang, outDir)
      } catch {
        case ex: Throwable =>
          SpecFailure(List(exceptionToCompileError(ex, classSpec.nameAsStr)))
      }
      classSpec.nameAsStr -> res
    }.toMap
  }

  def compileSpecAndWriteToFile(
    spec: ClassSpec,
    lang: LanguageCompilerStatic,
    outDir: String
  ): SpecSuccess = {
    val res = Main.compile(spec, lang, config.runtime)
    res.files.foreach { (file) =>
      Log.fileOps.info(() => s".... writing ${file.fileName}")

      val outPath = new File(outDir + "/" + file.fileName)

      // Ensure that all directories leading to this path exist
      val parentPath = outPath.getParentFile
      parentPath.mkdirs

      val fw = new FileWriter(outPath)
      fw.write(file.contents)
      fw.close()
    }
    res
  }

  private def exceptionToCompileError(ex: Throwable, srcFile: String): CompileError = {
    if (!config.jsonOutput)
      Console.println(ex.getMessage)
    ex match {
      case ype: YAMLParseException =>
        CompileError("(main)", ype.path, ype.msg)
      case _ =>
        CompileError(srcFile, List(), ex.getMessage)
    }
  }
}
