package io.kaitai.struct

import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.net.URLDecoder
import io.kaitai.struct.CompileLog._
import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.format.{ClassSpec, ClassSpecs, KSVersion}
import io.kaitai.struct.formats.JavaKSYParser
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.languages.components.LanguageCompilerStatic
import io.kaitai.struct.problems.{CompilationProblem, CompilationProblemException, ErrorInInput}
import org.rogach.scallop._

object JavaMain {
  KSVersion.current = Version.version

  case class CLIConfig(
    verbose: Seq[String] = Seq(),
    srcFiles: Seq[String] = Seq(),
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

  class KSCConf(args: Seq[String]) extends ScallopConf(args) {
    version(s"${Version.name} ${Version.version}")
    banner(s"""${Version.name} ${Version.version}
              |
              |Usage: kaitai-struct-compiler [options] <file>...
              |
              |Options:
              |""".stripMargin)
    footer("\nFor more information, see https://kaitai.io/")

    val srcFiles = trailArg[List[String]](
      name = "file",
      descr = "source files (.ksy)",
      required = false,
      default = Some(List())
    )

    val target = opt[List[String]](
      short = 't',
      name = "target",
      descr = s"target languages (${VALID_LANGS.mkString(", ")})",
      required = true,
      validate = targets => {
        targets.forall(t => VALID_LANGS.contains(t))
      }
    )

    val outdir = opt[File](
      short = 'd',
      name = "outdir",
      descr = "output directory (filenames will be auto-generated); on Unix-like shells, the short form `-d` requires arguments to be preceded by `--`",
      default = Some(new File("."))
    )

    val importPath = opt[List[String]](
      short = 'I',
      name = "import-path",
      descr = ".ksy library search path(s) for imports (see also KSPATH env variable)",
      default = Some(List())
    )

    val readWrite = opt[Boolean](
      short = 'w',
      name = "read-write",
      descr = "generate read-write support in classes (implies `--no-auto-read --zero-copy-substream false`, Java and Python only, default: read-only)",
      default = Some(false),
      noshort = true
    )

    val cppNamespace = opt[String](
      name = "cpp-namespace",
      descr = "C++ namespace (C++ only, default: none)",
      default = None
    )

    val cppStandard = opt[String](
      name = "cpp-standard",
      descr = "C++ standard to target (C++ only, supported: 98, 11, default: 98)",
      default = None,
      validate = std => std.isEmpty || CPP_STANDARDS.contains(std)
    )

    val goPackage = opt[String](
      name = "go-package",
      descr = "Go package (Go only, default: none)",
      default = None
    )

    val javaPackage = opt[String](
      name = "java-package",
      descr = "Java package (Java only, default: root package)",
      default = None
    )

    val javaFromFileClass = opt[String](
      name = "java-from-file-class",
      descr = s"Java class to be invoked in fromFile() helper (default: ${RuntimeConfig().java.fromFileClass})",
      default = None
    )

    val dotnetNamespace = opt[String](
      name = "dotnet-namespace",
      descr = ".NET Namespace (.NET only, default: Kaitai)",
      default = None
    )

    val phpNamespace = opt[String](
      name = "php-namespace",
      descr = "PHP Namespace (PHP only, default: root package)",
      default = None
    )

    val pythonPackage = opt[String](
      name = "python-package",
      descr = "Python package (Python only, default: root package)",
      default = None
    )

    val nimModule = opt[String](
      name = "nim-module",
      descr = "Path of Nim runtime module (Nim only, default: kaitai_struct_nim_runtime)",
      default = None
    )

    val nimOpaque = opt[String](
      name = "nim-opaque",
      descr = "Directory of opaque Nim modules (Nim only, default: directory of generated module)",
      default = None
    )

    val opaqueTypes = opt[Boolean](
      name = "opaque-types",
      descr = "opaque types allowed, default: false",
      default = Some(false)
    )

    val zeroCopySubstream = opt[Boolean](
      name = "zero-copy-substream",
      descr = "zero-copy substreams allowed, default: true",
      default = Some(true)
    )

    val kscExceptions = opt[Boolean](
      name = "ksc-exceptions",
      descr = "ksc throws exceptions instead of human-readable error messages",
      default = Some(false),
      noshort = true
    )

    val kscJsonOutput = opt[Boolean](
      name = "ksc-json-output",
      descr = "output compilation results as JSON to stdout",
      default = Some(false),
      noshort = true
    )

    val verbose = opt[List[String]](
      name = "verbose",
      descr = "verbose output",
      default = Some(List()),
      validate = verboses => {
        verboses.forall(v => v == "all" || Log.VALID_SUBSYS.contains(v))
      }
    )

    val noAutoRead = opt[Boolean](
      name = "no-auto-read",
      descr = "disable auto-running `_read` in constructor",
      default = Some(false),
      noshort = true
    )

    val readPos = opt[Boolean](
      name = "read-pos",
      descr = "`_read` remembers attribute positions in stream",
      default = Some(false),
      noshort = true
    )

    val debug = opt[Boolean](
      name = "debug",
      descr = "same as --no-auto-read --read-pos (useful for visualization tools)",
      default = Some(false),
      noshort = true
    )

    verify()
  }

  def parseCommandLine(args: Array[String]): Option[CLIConfig] = {
    try {
      val conf = new KSCConf(args)

      // Expand targets
      val expandedTargets = conf.target().flatMap { t =>
        if (t == "all") {
          ALL_LANGS.toSeq
        } else {
          Seq(t)
        }
      }

      // Expand verbose flags
      val expandedVerbose = conf.verbose().flatMap { v =>
        if (v == "all") {
          Log.VALID_SUBSYS
        } else {
          Seq(v)
        }
      }

      // Split import paths by path separator
      val expandedImportPaths = conf.importPath().flatMap(_.split(File.pathSeparatorChar))

      // Build runtime config
      var runtime = RuntimeConfig()

      // Apply read-write flag
      if (conf.readWrite()) {
        runtime = runtime.copy(readWrite = true, autoRead = false)
      }

      // Apply C++ options
      conf.cppNamespace.toOption.foreach { ns =>
        runtime = runtime.copy(
          cppConfig = runtime.cppConfig.copy(namespace = ns.split("::").toList)
        )
      }

      conf.cppStandard.toOption.foreach {
        case "98" =>
          runtime = runtime.copy(cppConfig = runtime.cppConfig.copyAsCpp98())
        case "11" =>
          runtime = runtime.copy(cppConfig = runtime.cppConfig.copyAsCpp11())
        case _ => // Should not happen due to validation
      }

      // Apply language-specific package options
      conf.goPackage.toOption.foreach { pkg =>
        runtime = runtime.copy(goPackage = pkg)
      }

      conf.javaPackage.toOption.foreach { pkg =>
        runtime = runtime.copy(java = runtime.java.copy(javaPackage = pkg))
      }

      conf.javaFromFileClass.toOption.foreach { cls =>
        runtime = runtime.copy(java = runtime.java.copy(fromFileClass = cls))
      }

      conf.dotnetNamespace.toOption.foreach { ns =>
        runtime = runtime.copy(dotNetNamespace = ns)
      }

      conf.phpNamespace.toOption.foreach { ns =>
        runtime = runtime.copy(phpNamespace = ns)
      }

      conf.pythonPackage.toOption.foreach { pkg =>
        runtime = runtime.copy(pythonPackage = pkg)
      }

      conf.nimModule.toOption.foreach { mod =>
        runtime = runtime.copy(nimModule = mod)
      }

      conf.nimOpaque.toOption.foreach { opq =>
        runtime = runtime.copy(nimOpaque = opq)
      }

      // Apply boolean options
      runtime = runtime.copy(opaqueTypes = conf.opaqueTypes())
      runtime = runtime.copy(zeroCopySubstream = conf.zeroCopySubstream())

      // Apply auto-read and read-pos options
      if (conf.noAutoRead() || conf.debug()) {
        runtime = runtime.copy(autoRead = false)
      }

      if (conf.readPos() || conf.debug()) {
        runtime = runtime.copy(readStoresPos = true)
      }

      // Build final config
      var config = CLIConfig(
        verbose = expandedVerbose,
        srcFiles = conf.srcFiles(),
        outDir = conf.outdir(),
        targets = expandedTargets,
        throwExceptions = conf.kscExceptions(),
        jsonOutput = conf.kscJsonOutput(),
        importPaths = expandedImportPaths,
        runtime = runtime
      )

      // Apply read-write zero-copy-substream override
      if (config.runtime.readWrite) {
        config = config.copy(runtime = config.runtime.copy(zeroCopySubstream = false))
      }

      Some(config)
    } catch {
      case e: exceptions.ScallopException =>
        Console.err.println(e.getMessage)
        None
      case e: Throwable =>
        Console.err.println(s"Error parsing command line: ${e.getMessage}")
        None
    }
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
    // $HOME/git/kaitai_struct/compiler/jvm/target/scala-2.13/classes/
    //
    // Linux, from stage:
    // $HOME/git/kaitai_struct/compiler/jvm/target/universal/stage/lib/io.kaitai.kaitai-struct-compiler-0.10-SNAPSHOT.jar
    //
    // Linux, from "sbt compilerJVM/run"
    // $HOME/git/kaitai_struct/compiler/jvm/target/scala-2.13/classes/
    //
    // Linux, from universal, custom install path:
    // /tmp/a%20b/kaitai-struct-compiler-0.10-SNAPSHOT/lib/io.kaitai.kaitai-struct-compiler-0.10-SNAPSHOT.jar
    //
    // Linux, from Debian install:
    // /usr/share/kaitai-struct-compiler/lib/io.kaitai.kaitai-struct-compiler-0.10-SNAPSHOT.jar
    //
    // Windows, default install path:
    // /C:/Program%20Files/kaitai-struct-compiler/lib/io.kaitai.kaitai-struct-compiler-0.10-SNAPSHOT.jar
    //
    // Windows, custom install path with spaces and non-latin chars:
    // /G:/%d0%b3%d0%b4%d0%b5-%d1%82%d0%be%20%d1%82%d0%b0%d0%bc/lib/io.kaitai.kaitai-struct-compiler-0.10-SNAPSHOT.jar

    try {
      optionOrLog(
        classOf[JavaMain].getProtectionDomain.getCodeSource,
        "home path: unable to run getCodeSource(), got null"
      ).flatMap(sourceCode => optionOrLog(
        sourceCode.getLocation,
        "home path: unable to run getLocation(), got null"
      )).flatMap(location => optionOrLog(
        location.getPath,
        "home path: unable to run getPath(), got null"
      )).flatMap(fStr => {
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
      })
    } catch {
      case se: SecurityException =>
        Log.importOps.info(() => s"home path: unable to run getProtectionDomain(), got SecurityException $se")
        None
    }
  }

  /**
   * Helper method to wrap nullable value (coming from Java API) into Option.
   * If it's null, we will bail out and won't process any longer due to a chain
   * of flatMap(), but if we use this method, we'll also note in our logging which
   * step failed, making it easier to diagnose.
   * @param nullableValue value which is potentially null
   * @param errMsg error message to show in case if it's null
   * @tparam T type of potentially nullable value
   * @return option-wrapped value
   * @see [[scala.Option.apply()]]
   */
  private def optionOrLog[T](nullableValue: T, errMsg: String): Option[T] =
    Option(nullableValue) match {
      case None =>
        Log.importOps.info(() => errMsg)
        None
      case someValue =>
        someValue
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
        compileOneInput(srcFile)
      } else {
        try {
          compileOneInput(srcFile)
        } catch {
          case ex: Throwable =>
            InputFailure(List(exceptionToCompileError(ex, srcFile)))
        }
      }
      if (!config.jsonOutput) {
        val problems = log match {
          case InputFailure(errors) => errors
          case InputSuccess(firstSpecName, output, precompileProblems) => precompileProblems
        }
        problems.foreach { (p) => Console.err.println(p.message) }
      }

      srcFile -> log
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

  private def compileOneInput(srcFile: String): InputEntry = {
    Log.fileOps.info(() => s"parsing $srcFile...")
    val (specsOpt, precompileProblems) = JavaKSYParser.localFileToSpecs(srcFile, config)

    specsOpt match {
      case Some(specs) =>
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
          output,
          precompileProblems
        )
      case None =>
        InputFailure(precompileProblems)
    }
  }

  def compileAllLangs(specs: ClassSpecs, config: CLIConfig): Map[String, Map[String, SpecEntry]] = {
    config.targets.map { langStr =>
      langStr -> (langStr match {
        case "go" | "java" =>
          compileOneLang(specs, langStr, s"${config.outDir}/${langStr}/src")
        case _ =>
          compileOneLang(specs, langStr, s"${config.outDir}/${langStr}")
      })
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
          val p = exceptionToCompileError(ex, classSpec.nameAsStr)
          if (!config.jsonOutput) {
            Console.err.println(p.message)
          }
          SpecFailure(List(p))
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

      val osw = new OutputStreamWriter(new FileOutputStream(outPath), StandardCharsets.UTF_8)
      osw.write(file.contents)
      osw.close()
    }
    res
  }

  private def exceptionToCompileError(ex: Throwable, srcFile: String): CompilationProblem = {
    ex match {
      case cpe: CompilationProblemException =>
        cpe.problem
      case _ =>
        // TODO: have a dedicated class instead of ErrorInInput
        ErrorInInput(ex, List(), Some(srcFile))
    }
  }
}
