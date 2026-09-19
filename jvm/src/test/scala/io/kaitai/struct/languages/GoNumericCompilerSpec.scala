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

class GoNumericCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  private val fixtures = Seq("numeric_widths", "numeric_mixed_division", "numeric_unary", "numeric_short_circuit")

  test("read-write rejects integer arrays without a lossless native representation") {
    Seq("unsigned_array_unrepresentable", "unsigned_array_unrepresentable_literal").foreach { fixture =>
      val error = intercept[io.kaitai.struct.precompile.TypeMismatchError] {
        compile(fixture, readWrite = true)
      }
      assert(error.getMessage == "Go read-write cannot represent an integer array containing both negative values and values above 9223372036854775807 without loss")
      assert(compile(fixture, readWrite = false).nonEmpty)
    }
  }

  test("read-write unsigned constants expose a representable native API") {
    val code = compile("unsigned_constants", readWrite = true)
    assert(code.contains("type UnsignedConstants_Values uint64"))
    assert(code.contains("Decimal() (v uint64, err error)"))
    assert(code.contains("SignedMax() (v int, err error)"))
    assert(code.contains("type UnsignedConstants_Nested_Signed int"))
    val readOnly = compile("unsigned_constants", readWrite = false)
    assert(readOnly.contains("type UnsignedConstants_Values int"))
    assert(readOnly.contains("Decimal() (v int, err error)"))
  }

  private def compile(fixture: String, readWrite: Boolean): String = {
    val file = new File(getClass.getResource(s"/go-read-write/$fixture.ksy").toURI)
    val runtime = RuntimeConfig(readWrite = readWrite, goPackage = "contract")
    val (specsOption, problems) = JavaKSYParser.localFileToSpecs(file.getPath, CLIConfig(runtime = runtime))
    val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
    Main.compile(specs, specs.firstSpec, GoCompiler, runtime).files.head.contents
  }

  for (readWrite <- Seq(false, true)) {
    test(s"numeric expressions generate in readWrite=$readWrite") {
      fixtures.foreach(fixture => assert(compile(fixture, readWrite).nonEmpty))
    }
    sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
      test(s"numeric expressions preserve widths, signedness and shifts in readWrite=$readWrite") {
        val dir = Files.createTempDirectory("kaitai-go-numeric-")
        def write(name: String, text: String): Unit = Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
        val runtime = new File(runtimePath).getCanonicalPath
        write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
        (fixtures ++ (if (readWrite) Seq("unsigned_constants", "unsigned_array_compositions") else Seq.empty)).foreach { fixture =>
          write(s"$fixture.go", compile(fixture, readWrite))
          val source = scala.io.Source.fromURL(getClass.getResource(s"/go-read-write/${fixture}_test.go"))
          try write(s"${fixture}_test.go", source.mkString) finally source.close()
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
