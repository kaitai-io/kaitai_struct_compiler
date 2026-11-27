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

class GoSessionCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version
  sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
    test("output sessions finish late instances and preserve reservation context") {
      val dir = Files.createTempDirectory("kaitai-go-session-")
      def write(name: String, text: String): Unit = { Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8)); () }
      def compile(file: File): Unit = {
        val config = RuntimeConfig(readWrite = true, goPackage = "contract")
        val (specsOption, problems) = JavaKSYParser.localFileToSpecs(file.getPath, CLIConfig(runtime = config))
        val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
        Main.compile(specs, specs.firstSpec, GoCompiler, config).files.foreach(f => write(new File(f.fileName).getName, f.contents))
      }
      Seq("instance_io_user", "instance_io_user_earlier", "process_repeat_usertype_dynarg_custom", "process_repeat_usertype_dynarg_rotate", "process_repeat_usertype_dynarg_xor").foreach(n => compile(new File(s"../tests/formats/$n.ksy")))
      val schemas = Seq(
        "session_bits" -> "meta:\n  id: session_bits\nseq:\n  - id: child\n    type: body\n    size: 1\n  - id: tail\n    type: b3\ntypes:\n  body:\n    seq:\n      - id: value\n        type: u1\n",
        "session_optional" -> "meta:\n  id: session_optional\nseq:\n  - id: child\n    type: body\n    terminator: 0\n    include: true\n    consume: false\n    eos-error: false\n  - id: tail\n    type: u1\ntypes:\n  body:\n    seq:\n      - id: value\n        type: u1\n    instances:\n      terminal:\n        pos: 1\n        type: u1\n",
        "session_key" -> "meta:\n  id: session_key\nparams:\n  - id: key\n    type: bytes\nseq:\n  - id: child\n    type: body\n    size: 1\n    process: xor(key)\n  - id: tail\n    type: u1\ntypes:\n  body:\n    seq:\n      - id: value\n        type: u1\n",
        "session_repeat_optional" -> "meta:\n  id: session_repeat_optional\nseq:\n  - id: children\n    type: body\n    terminator: 0\n    include: true\n    consume: false\n    eos-error: false\n    repeat: expr\n    repeat-expr: 2\ntypes:\n  body:\n    seq:\n      - id: data\n        size-eos: true\n",
        "session_external" -> "meta:\n  id: session_external\nparams:\n  - id: output\n    type: io\nseq:\n  - id: value\n    type: u1\ninstances:\n  part:\n    io: output\n    pos: 0\n    size: 1\n    type: body\ntypes:\n  body:\n    seq:\n      - id: value\n        type: u1\n",
        "session_order" -> "meta:\n  id: session_order\nseq:\n  - id: value\n    type: u1\ninstances:\n  a:\n    pos: 0\n    type: body\n  b:\n    pos: 0\n    type: u1\ntypes:\n  body:\n    instances:\n      mark:\n        io: _root._io\n        pos: 0\n        type: u1\n"
      )
      schemas.foreach { case (name, schema) => write(s"$name.ksy", schema); compile(dir.resolve(s"$name.ksy").toFile) }
      Seq("write_optional_included_peek", "write_optional_included", "write_optional_peek").foreach(n => compile(new File(getClass.getResource(s"/go-read-write/$n.ksy").toURI)))
      val runtime = new File(runtimePath).getCanonicalPath
      write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
      Seq("session_test.go", "session_external_test.go").foreach { name =>
        val source = scala.io.Source.fromURL(getClass.getResource(s"/go-read-write/$name"))
        try write(name, source.mkString) finally source.close()
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
