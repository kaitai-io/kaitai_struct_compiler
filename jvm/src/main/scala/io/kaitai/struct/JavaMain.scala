package io.kaitai.struct

import java.io.{File, FileWriter}
import java.net.URLDecoder

import io.kaitai.struct.CompileLog._
import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.{ClassSpec, ClassSpecs, KSVersion, YAMLParseException}
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.languages.components.LanguageCompilerStatic
import io.kaitai.struct.precompile.ErrorInInput

object JavaMain {
  KSVersion.current = Version.version

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

  val ALL_LANGS = LanguageCompilerStatic.NAME_TO_CLASS.keySet - "cpp_stl" + "cpp_stl_98" + "cpp_stl_11"
  val VALID_LANGS = LanguageCompilerStatic.NAME_TO_CLASS.keySet + "all"
  val CPP_STANDARDS = Set("98", "11")

  def parseCommandLine(args: Array[String]): Option[CLIConfig] = {
    val parser = new scopt.OptionParser[CLIConfig](Version.name) {
      override def showUsageOnError = true

      head(Version.name, Version.version)

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
          failure(s"'$x' is not a valid target language; valid ones are: ${VALID_LANGS.mkString(", ")}")
        }
      }

      opt[File]('d', "outdir") valueName("<directory>") action { (x, c) =>
        c.copy(outDir = x)
      } text("output directory (filenames will be auto-generated)")

      val importPathExample = List("<directory>", "<directory>", "...").mkString(File.pathSeparator)
      opt[String]('I', "import-path") optional() unbounded() valueName(importPathExample) action { (x, c) =>
        c.copy(importPaths = c.importPaths ++ x.split(File.pathSeparatorChar))
      } text(".ksy library search path(s) for imports (see also KSPATH env variable)")

      opt[String]("cpp-namespace") valueName("<namespace>") action { (x, c) =>
        c.copy(
          runtime = c.runtime.copy(
            cppConfig = c.runtime.cppConfig.copy(
              namespace = x.split("::").toList
            )
          )
        )
      } text("C++ namespace (C++ only, default: none)")

      opt[String]("cpp-standard") valueName("<standard>") action { (x, c) =>
        c.copy(
          runtime = c.runtime.copy(
            cppConfig = x match {
              case "98" => c.runtime.cppConfig.copyAsCpp98()
              case "11" => c.runtime.cppConfig.copyAsCpp11()
            }
          )
        )
      } text("C++ standard to target (C++ only, supported: 98, 11, default: 98)") validate { x =>
        if (CPP_STANDARDS.contains(x)) {
          success
        } else {
          failure(s"'$x' is not a valid C++ standard to target; valid ones are: ${CPP_STANDARDS.mkString(", ")}")
        }
      }

      opt[String]("go-package") valueName("<package>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(goPackage = x))
      } text("Go package (Go only, default: none)")

      opt[String]("java-package") valueName("<package>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(javaPackage = x))
      } text("Java package (Java only, default: root package)")

      opt[String]("java-from-file-class") valueName("<class>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(javaFromFileClass = x))
      } text(s"Java class to be invoked in fromFile() helper (default: ${RuntimeConfig().javaFromFileClass})")

      opt[String]("dotnet-namespace") valueName("<namespace>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(dotNetNamespace = x))
      } text(".NET Namespace (.NET only, default: Kaitai)")

      opt[String]("php-namespace") valueName("<namespace>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(phpNamespace = x))
      } text("PHP Namespace (PHP only, default: root package)")

      opt[String]("python-package") valueName("<package>") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(pythonPackage = x))
      } text("Python package (Python only, default: root package)")

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

      opt[Unit]("no-auto-read") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(autoRead = false))
      } text("disable auto-running `_read` in constructor")

      opt[Unit]("read-pos") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(readStoresPos = true))
      } text("`_read` remembers attribute positions in stream")

      opt[Unit]("debug") action { (x, c) =>
        c.copy(runtime = c.runtime.copy(autoRead = false, readStoresPos = true))
      } text("same as --no-auto-read --read-pos (useful for visualization tools)")

      help("help") text("display this help and exit")
      version("version") text("output version information and exit")
    }

    parser.parse(args, CLIConfig())
  }

  /**
    * Insanely hacky method that relies on some JVM black magic to get
    * application "home". From that, we can check if it has a "formats" subdir,
    * and if it does, consider that an additional default ksy imports path.
    * @return additional import search path ("default", platform-specific one),
    *         if it's found
    */
  private def homePath: Option[String] = {
    // fStr is mysterious and probably unreliable, but that's the best I've
    // came up with. It uses java.security API, in which CodeSource is used
    // to have indicative "code location", but AFAIK, in Sun/Oracle applications
    // it is only used as URL for comparison purposes. It is URL-encoded, so
    // we need to decode it as well.
    //
    // Linux, from IDE:
    // $HOME/git/kaitai_struct/compiler/jvm/target/scala-2.11/classes/
    //
    // Linux, from stage:
    // $HOME/git/kaitai_struct/compiler/jvm/target/universal/stage/lib/io.kaitai.kaitai-struct-compiler-0.7-SNAPSHOT.jar
    //
    // Linux, from "sbt compilerJVM/run"
    // $HOME/git/kaitai_struct/compiler/jvm/target/scala-2.11/classes/
    //
    // Linux, from universal, custom install path:
    // /tmp/a%20b/kaitai-struct-compiler-0.7-SNAPSHOT/lib/io.kaitai.kaitai-struct-compiler-0.7-SNAPSHOT.jar
    //
    // Linux, from Debian install:
    // /usr/share/kaitai-struct-compiler/lib/io.kaitai.kaitai-struct-compiler-0.7-SNAPSHOT.jar
    //
    // Windows, default install path:
    // /C:/Program%20Files/kaitai-struct-compiler/lib/io.kaitai.kaitai-struct-compiler-0.7-SNAPSHOT.jar
    //
    // Windows, custom install path with spaces and non-latin chars:
    // /G:/%d0%b3%d0%b4%d0%b5-%d1%82%d0%be%20%d1%82%d0%b0%d0%bc/lib/io.kaitai.kaitai-struct-compiler-0.7-SNAPSHOT.jar

    val fStr = classOf[JavaMain].getProtectionDomain.getCodeSource.getLocation.getPath
    Log.importOps.info(() => s"home path: location = $fStr")

    if (fStr.endsWith(".jar")) {
      val fDec = URLDecoder.decode(fStr, "UTF-8")
      Log.importOps.info(() => s"... URL-decoded = $fDec")

      val homeFile = new File(fDec).getParentFile.getParentFile
      Log.importOps.info(() => s"... home = $homeFile")

      if (homeFile.exists) {
        val homeFormat = new File(homeFile, "formats")
        Log.importOps.info(() => s"... formats = $homeFormat")
        if (homeFormat.exists) {
          Some(homeFormat.toString)
        } else {
          Log.importOps.info(() => "... home formats dir doesn't exist => fail")
          None
        }
      } else {
        Log.importOps.info(() => s"... home doesn't exist => no home import paths")
        None
      }
    } else {
      Log.importOps.info(() => s"... not a jar, we're not running a packaged app => no home")
      None
    }
  }

  private def envPaths: List[String] =
    sys.env.get("KSPATH").toList.flatMap((x) => x.split(File.pathSeparatorChar))

  def main(args: Array[String]): Unit = {
    parseCommandLine(args) match {
      case None => System.exit(1)
      case Some(config0) =>
        Log.initFromVerboseFlag(config0.verbose)
        val config = config0.copy(importPaths = config0.importPaths ++ envPaths ++ homePath)
        new JavaMain(config).run()
    }
  }
}

class JavaMain(config: CLIConfig) {
  import JavaMain._

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

    if (config.jsonOutput) {
      Console.println(JSON.mapToJson(logs))
    } else {
      if (logsHaveErrors(logs))
        System.exit(2)
    }
  }

  private def logsHaveErrors(logs: Map[String, InputEntry]): Boolean =
    logs.values.map(_.hasErrors).max

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

    val (lang, fixedRuntime) = langStr match {
      case "cpp_stl_98" =>
        (CppCompiler, config.runtime.copy(cppConfig = config.runtime.cppConfig.copyAsCpp98()))
      case "cpp_stl_11" =>
        (CppCompiler, config.runtime.copy(cppConfig = config.runtime.cppConfig.copyAsCpp11()))
      case _ =>
        (LanguageCompilerStatic.byString(langStr), config.runtime)
    }

    specs.map { case (_, classSpec) =>
      val res = try {
        compileSpecAndWriteToFile(specs, classSpec, lang, fixedRuntime, outDir)
      } catch {
        case ex: Throwable =>
          if (config.throwExceptions)
            ex.printStackTrace()
          SpecFailure(List(exceptionToCompileError(ex, classSpec.nameAsStr)))
      }
      classSpec.nameAsStr -> res
    }.toMap
  }

  def compileSpecAndWriteToFile(
    specs: ClassSpecs,
    spec: ClassSpec,
    lang: LanguageCompilerStatic,
    runtime: RuntimeConfig,
    outDir: String
  ): SpecSuccess = {
    val res = Main.compile(specs, spec, lang, runtime)
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
      Console.err.println(ex.getMessage)
    ex match {
      case ype: YAMLParseException =>
        CompileError("(main)", ype.path, ype.msg)
      case e: ErrorInInput =>
        val file = e.file.getOrElse(srcFile)
        val msg = Option(e.getCause) match {
          case Some(cause) => cause.getMessage
          case None => e.getMessage
        }
        CompileError(file, e.path, msg)
      case _ =>
        CompileError(srcFile, List(), ex.getMessage)
    }
  }
}
