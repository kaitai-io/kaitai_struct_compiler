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

class GoRawPositionCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version
  sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
    test("raw processors observe the consumed encoded region and restore the output cursor") {
      val dir = Files.createTempDirectory("kaitai-go-raw-position-")
      def write(name: String, text: String): Unit = { Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8)); () }
      def schema(name: String, body: String): Unit = {
        val path = dir.resolve(s"$name.ksy")
        write(s"$name.ksy", s"meta:\n  id: $name\n$body")
        val config = RuntimeConfig(readWrite = true, goPackage = "contract")
        val (specsOption, problems) = JavaKSYParser.localFileToSpecs(path.toString, CLIConfig(runtime = config))
        val specs = specsOption.getOrElse(fail(problems.mkString(", ")))
        Main.compile(specs, specs.firstSpec, GoCompiler, config).files.foreach(f => write(new File(f.fileName).getName, f.contents))
      }
      schema("raw_position", "seq:\n  - id: data\n    size: 1\n    process: xor(_io.pos)\n")
      schema("raw_width", "params:\n  - id: width\n    type: u4\nseq:\n  - id: data\n    size: width\n    process: xor(_io.pos)\n")
      schema("raw_eos", "seq:\n  - id: data\n    size-eos: true\n    process: xor(_io.pos)\n")
      schema("raw_rotate", "seq:\n  - id: data\n    size: 1\n    process: ror(_io.pos)\n")
      schema("raw_custom", "seq:\n  - id: data\n    size: 2\n    process: raw_codec(_io.pos)\n")
      schema("raw_failure", "seq:\n  - id: data\n    size: 1\n    process: failing_codec(_io.pos)\n")
      schema("raw_error_bits", "seq:\n  - id: prefix\n    type: b3\n  - id: data\n    size: 1\n    process: failing_codec(_io.pos)\n")
      schema("raw_eos_custom", "seq:\n  - id: data\n    size-eos: true\n    pad-right: 255\n    process: raw_codec(_io.pos)\n")
      schema("raw_repeat_custom", "seq:\n  - id: data\n    size: 1\n    repeat: expr\n    repeat-expr: 2\n    process: failing_codec(_io.pos + _index)\n")
      schema("raw_until", "seq:\n  - id: data\n    size: 1\n    repeat: until\n    repeat-until: _[0] == 81\n    process: xor(_io.pos)\n")
      schema("raw_external", "params:\n  - id: output\n    type: io\nseq:\n  - id: value\n    type: u1\ninstances:\n  part:\n    io: output\n    pos: 1\n    size: 1\n    process: xor(output.pos + _io.pos)\n")
      schema("raw_param", "params:\n  - id: output\n    type: io\nseq:\n  - id: data\n    size: 1\n    process: xor(output.pos)\n")
      schema("raw_capture", "params:\n  - id: widths\n    type: bytes\nseq:\n  - id: data\n    size: widths[0]\n    pad-right: 255\n    process: mutate_codec(_io.pos, widths)\n")
      schema("raw_bad_size", "params:\n  - id: width\n    type: s8\nseq:\n  - id: data\n    size: width\n    process: xor(_io.pos)\n")
      schema("raw_bad_size_custom", "params:\n  - id: width\n    type: s8\nseq:\n  - id: data\n    size: width\n    process: failing_codec(_io.pos)\n")
      schema("raw_key_size", "seq:\n  - id: data\n    size: 1\n    process: xor(key)\ninstances:\n  key:\n    value: _io.size\n")
      schema("raw_switch", "params:\n  - id: tag\n    type: u1\nseq:\n  - id: data\n    size: 1\n    process: failing_codec(_io.pos)\n    type:\n      switch-on: tag\n      cases:\n        1: u1\n")
      schema("raw_term_static", "seq:\n  - id: data\n    terminator: 0\n    process: failing_codec(1)\n")
      schema("raw_term_layout", "seq:\n  - id: data\n    terminator: 0\n    eos-error: false\n    process: xor(_io.pos + key)\ninstances:\n  key:\n    pos: 3\n    type: u1\n")
      for (include <- Seq(false, true)) {
        schema(s"raw_bounded_term_$include", s"seq:\n  - id: data\n    size: 3\n    terminator: 0\n    include: $include\n    pad-right: 255\n    process: xor(_io.pos)\n")
      }
      schema("raw_repeat", "seq:\n  - id: data\n    size: 1\n    repeat: expr\n    repeat-expr: 2\n    process: xor(_io.pos + _index)\n")
      schema("raw_bits", "seq:\n  - id: prefix\n    type: b3\n  - id: data\n    size: 1\n    process: xor(_io.pos)\n  - id: tail\n    type: b5\n")
      schema("raw_lazy", "seq:\n  - id: data\n    size: 1\n    process: xor(key)\ninstances:\n  key:\n    value: _io.pos\n")
      schema("raw_size", "seq:\n  - id: data\n    size: 3 - _io.pos\n    pad-right: 255\n    process: xor(_io.pos)\n")
      schema("raw_nested", "seq:\n  - id: prefix\n    type: u1\n  - id: child\n    type: body\n    size: 2\ntypes:\n  body:\n    seq:\n      - id: data\n        size-eos: true\n        process: xor(_io.pos + _parent._io.pos)\n")
      schema("raw_choice", "params:\n  - id: enabled\n    type: bool\nseq:\n  - id: data\n    if: enabled\n    size: 1\n    process: 'failing_codec(enabled ? _io.pos : _io.size)'\n")
      for (include <- Seq(false, true); consume <- Seq(false, true); optional <- Seq(false, true)) {
        schema(s"raw_term_${include}_${consume}_$optional", s"seq:\n  - id: data\n    terminator: 0\n    include: $include\n    consume: $consume\n    eos-error: ${!optional}\n    process: xor(_io.pos)\n")
      }
      schema("raw_term_custom", "seq:\n  - id: data\n    terminator: 0\n    process: raw_codec(_io.pos)\n")
      val runtime = new File(runtimePath).getCanonicalPath
      write("go.mod", s"module contract\n\ngo 1.24.0\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
      val source = scala.io.Source.fromURL(getClass.getResource("/go-read-write/raw_position_test.go"))
      try write("raw_position_test.go", source.mkString) finally source.close()
      val output = new StringBuilder
      def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
      val logger = ProcessLogger(log, log)
      assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
      assert(Process(Seq("go", "test", "-mod=mod", "-count=1", "-v", "./..."), dir.toFile).!(logger) == 0, s"Generated Go in $dir failed:\n$output")
      info(s"Generated Go in $dir:\n$output")
    }
  }
}
