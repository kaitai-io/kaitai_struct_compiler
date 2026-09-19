package io.kaitai.struct.languages

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.{Main, RuntimeConfig, Version}
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.sys.process._

class GoParentCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  private def compile(readWrite: Boolean, fixture: String = "parent_navigation"): String = {
    val file = new File(getClass.getResource(s"/go-read-write/$fixture.ksy").toURI)
    val runtime = RuntimeConfig(readWrite = readWrite, goPackage = "contract")
    val (specsOption, problems) = JavaKSYParser.localFileToSpecs(file.getPath, CLIConfig(runtime = runtime))
    val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
    Main.compile(specs, specs.firstSpec, GoCompiler, runtime).files.head.contents
  }

  for (readWrite <- Seq(false, true)) {
    test(s"generic parent navigation generates in readWrite=$readWrite") {
      assert(compile(readWrite).nonEmpty)
    }
    sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
      test(s"generic parents retain identity and fail safely in readWrite=$readWrite") {
        val dir = Files.createTempDirectory("kaitai-go-parent-")
        def write(name: String, text: String): Unit = Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
        val runtime = new File(runtimePath).getCanonicalPath
        write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
        write("parent_navigation.go", compile(readWrite))
        // Compile separately: a parent may come from any generated schema,
        // including one that contains no generic traversal of its own.
        write("parent_navigation_external.go", compile(readWrite, "parent_navigation_external"))
        val source = scala.io.Source.fromURL(getClass.getResource("/go-read-write/parent_navigation_test.go"))
        val common = try source.mkString finally source.close()
        val fields = Seq("Main", "Forced", "Buf", "Branch")
        write("parent_navigation_test.go", if (readWrite) fields.foldLeft(common)((s, f) => s.replace(s".$f", s".Get$f()")) else common)
        if (readWrite) {
          val source = scala.io.Source.fromURL(getClass.getResource("/go-read-write/parent_navigation_write_test.go"))
          try write("parent_navigation_write_test.go", source.mkString) finally source.close()
        }
        val output = new StringBuilder
        def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
        val logger = ProcessLogger(log, log)
        assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
        assert(Process(Seq("go", "test", "-mod=mod", "-v", "./..."), dir.toFile).!(logger) == 0, s"Generated Go in $dir failed:\n$output")
        info(s"Generated Go in $dir:\n$output")
      }
    }
  }
}
