package io.kaitai.struct.languages

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.{JavaMain, Main, RuntimeConfig, Version}
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.regex.Pattern.quote
import scala.sys.process._

/** Contract tests for the output of the public parsing/precompilation/generation pipeline.
  * These intentionally use small structural assertions rather than full-file snapshots.
  * These surface checks do not replace runtime conformance tests.
  */
class GoReadWriteCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

  test("read-write help lists every supported target") {
    val output = new ByteArrayOutputStream()
    val stream = new PrintStream(output, true, StandardCharsets.UTF_8.name())
    val result = Console.withOut(stream) {
      Console.withErr(stream) {
        // An invalid target prints the same usage text without the --help
        // action terminating the forked test JVM.
        JavaMain.parseCommandLine(Array("--target", "not-a-language"))
      }
    }
    stream.close()

    assert(result.isEmpty)
    val help = output.toString(StandardCharsets.UTF_8.name())
    assert(help.contains("Java, Python and Go"), s"read-write targets missing from help:\n$help")
  }

  private def compile(fixture: String, readWrite: Boolean = true): String = {
    val resource = Option(getClass.getResource(s"/go-read-write/$fixture.ksy"))
      .getOrElse(fail(s"Missing fixture: $fixture.ksy"))
    compileFile(new File(resource.toURI), readWrite)
  }

  private def compileContents(contents: String): String = {
    val file = Files.createTempFile("go-rw-contract-", ".ksy")
    try {
      Files.write(file, contents.getBytes(StandardCharsets.UTF_8))
      compileFile(file.toFile, readWrite = true)
    } finally {
      Files.deleteIfExists(file)
    }
  }

  private def compileFile(file: File, readWrite: Boolean): String = {
    val runtime = RuntimeConfig(readWrite = readWrite, goPackage = "contract")
    val (specsOption, problems) = JavaKSYParser.localFileToSpecs(
      file.getPath, CLIConfig(runtime = runtime)
    )
    val specs = specsOption.getOrElse(fail(s"Invalid fixture $file: ${problems.mkString(", ")}"))
    val files = try {
      Main.compile(specs, specs.firstSpec, GoCompiler, runtime).files
    } catch {
      case e: NotImplementedError if readWrite =>
        fail(s"Missing generated behavior: Go read-write compiler hook for $file", e)
    }
    assert(files.size == 1, s"Expected one generated Go file for $file")
    files.head.contents
  }

  private def contains(code: String, pattern: String, description: String): Unit =
    assert(pattern.r.findFirstIn(code).nonEmpty, s"Missing generated behavior: $description")

  private def method(code: String, owner: String, name: String, signature: String): String = {
    val header = (s"(?m)^func\\s+\\(\\w+\\s+\\*${quote(owner)}\\)\\s+${quote(name)}" + signature + "\\s*\\{").r
    val found = header.findFirstMatchIn(code)
      .getOrElse(fail(s"Missing generated behavior: $owner.$name$signature"))
    // All Go methods are top level, so the next declaration bounds the body even
    // when it contains nested conditionals. Do not depend on indentation.
    val end = "(?m)^func\\s".r.findFirstMatchIn(code.substring(found.end))
      .map(m => found.end + m.start).getOrElse(code.length)
    code.substring(found.end, end)
  }

  private val errorResult = "\\s+(?:error|\\(\\s*\\w+\\s+error\\s*\\))"

  private val scalarTypes = Seq("u1", "s1") ++
    (for (endian <- Seq("be", "le"); kind <- Seq("u", "s"); width <- Seq(2, 4, 8)) yield s"$kind$width$endian") ++
    (for (endian <- Seq("be", "le"); width <- Seq(4, 8)) yield s"f$width$endian")

  private val writeFixtures: Seq[(String, String)] = Seq(
    "write_scalars" -> ("meta:\n  id: write_scalars\nseq:\n" + scalarTypes.map(t => s"  - id: v_$t\n    type: $t\n").mkString),
    "write_bytes" -> """meta:
      |  id: write_bytes
      |seq:
      |  - id: fixed
      |    size: 2
      |  - id: padded
      |    size: 4
      |    pad-right: 255
      |  - id: terminated
      |    terminator: 0
      |  - id: included
      |    terminator: 0
      |    include: true
      |  - id: peeked
      |    terminator: 0
      |    consume: false
      |  - id: marker
      |    type: u1
      |  - id: padded_term
      |    size: 4
      |    terminator: 0
      |    pad-right: 255
      |  - id: padded_included
      |    size: 4
      |    terminator: 0
      |    include: true
      |  - id: tail
      |    size-eos: true
      |""".stripMargin,
    "write_strings" -> """meta:
      |  id: write_strings
      |seq:
      |  - id: latin
      |    type: str
      |    size: 1
      |    encoding: ISO-8859-1
      |  - id: utf
      |    type: str
      |    size: 2
      |    encoding: UTF-8
      |  - id: wide
      |    type: str
      |    size: 2
      |    encoding: UTF-16LE
      |""".stripMargin,
    "write_ascii" -> """meta:
      |  id: write_ascii
      |seq:
      |  - id: prefix
      |    type: u1
      |  - id: text
      |    type: str
      |    size: 2
      |    encoding: ASCII
      |  - id: suffix
      |    type: u1
      |""".stripMargin,
    "write_control" -> """meta:
      |  id: write_control
      |  endian: le
      |seq:
      |  - id: tag
      |    type: u1
      |    enum: tags
      |  - id: conditional
      |    type: u2
      |    if: tag == tags::one
      |  - id: switched
      |    type:
      |      switch-on: tag
      |      cases:
      |        'tags::one': u2
      |        _: u4
      |  - id: integer_switch
      |    type:
      |      switch-on: tag.to_i
      |      cases:
      |        1: u1
      |        _: u2
      |  - id: fixed
      |    type: u1
      |    repeat: expr
      |    repeat-expr: 2
      |  - id: until
      |    type: u1
      |    repeat: until
      |    repeat-until: _ == 0
      |  - id: eos
      |    type: u1
      |    repeat: eos
      |enums:
      |  tags:
      |    1: one
      |    2: two
      |""".stripMargin,
    "write_mixed_switch" -> """meta:
      |  id: write_mixed_switch
      |seq:
      |  - id: tag
      |    type: u1
      |  - id: value
      |    type:
      |      switch-on: tag
      |      cases:
      |        1: u1
      |    size: 2
      |""".stripMargin,
    "write_bits_be" -> """meta:
      |  id: write_bits_be
      |  bit-endian: be
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: bits
      |    type: b3
      |  - id: marker
      |    type: u1
      |  - id: tail
      |    type: b3
      |""".stripMargin,
    "write_bits_le" -> """meta:
      |  id: write_bits_le
      |  bit-endian: le
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: bits
      |    type: b3
      |  - id: marker
      |    type: u1
      |  - id: tail
      |    type: b3
      |""".stripMargin,
    "write_sized" -> """meta:
      |  id: write_sized
      |seq:
      |  - id: data
      |    size: _io.size
      |""".stripMargin,
    "write_root_sized" -> """meta:
      |  id: write_root_sized
      |seq:
      |  - id: data
      |    size: _root._io.size
      |""".stripMargin,
    "write_eos_term" -> """meta:
      |  id: write_eos_term
      |seq:
      |  - id: data
      |    terminator: 0
      |    eos-error: false
      |""".stripMargin,
    "write_eos_padding" -> """meta:
      |  id: write_eos_padding
      |seq:
      |  - id: header
      |    type: u1
      |  - id: data
      |    size-eos: true
      |    pad-right: 255
      |""".stripMargin,
    "write_included_optional" -> """meta:
      |  id: write_included_optional
      |seq:
      |  - id: data
      |    terminator: 0
      |    include: true
      |    eos-error: false
      |""".stripMargin,
    "write_bit_context" -> """meta:
      |  id: write_bit_context
      |seq:
      |  - id: prefix
      |    type: b3
      |  - id: suffix
      |    type: b5
      |    if: _io.size == 1
      |""".stripMargin,
    "write_bit_eos" -> """meta:
      |  id: write_bit_eos
      |seq:
      |  - id: values
      |    type: b1
      |    repeat: eos
      |""".stripMargin,
    "write_bit_eos_b4" -> """meta:
      |  id: write_bit_eos_b4
      |seq:
      |  - id: values
      |    type: b4
      |    repeat: eos
      |""".stripMargin,
    "write_bit_eof_boundary" -> """meta:
      |  id: write_bit_eof_boundary
      |  bit-endian: be
      |seq:
      |  - id: foo
      |    type: b20
      |  - id: bar
      |    type: b4
      |    if: not _io.eof
      |  - id: baz
      |    type: b16
      |    if: not _io.eof
      |  - id: qux
      |    type: b16
      |    if: not _io.eof
      |""".stripMargin,
    "write_bit_query_eof_be" -> """meta:
      |  id: write_bit_query_eof_be
      |  bit-endian: be
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: trailing
      |    type: u1
      |    if: not _io.eof
      |""".stripMargin,
    "write_bit_query_eof_le" -> """meta:
      |  id: write_bit_query_eof_le
      |  bit-endian: le
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: trailing
      |    type: u1
      |    if: not _io.eof
      |""".stripMargin,
    "write_bit_query_pos_be" -> """meta:
      |  id: write_bit_query_pos_be
      |  bit-endian: be
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: trailing
      |    type: u1
      |    if: _io.pos == 1
      |""".stripMargin,
    "write_bit_query_pos_le" -> """meta:
      |  id: write_bit_query_pos_le
      |  bit-endian: le
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: trailing
      |    type: u1
      |    if: _io.pos == 1
      |""".stripMargin,
    "write_bit_query_size_be" -> """meta:
      |  id: write_bit_query_size_be
      |  bit-endian: be
      |params:
      |  - id: expected_size
      |    type: u1
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: trailing
      |    type: u1
      |    if: _io.size == expected_size
      |""".stripMargin,
    "write_bit_query_size_le" -> """meta:
      |  id: write_bit_query_size_le
      |  bit-endian: le
      |params:
      |  - id: expected_size
      |    type: u1
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: trailing
      |    type: u1
      |    if: _io.size == expected_size
      |""".stripMargin,
    "write_bit_query_size_eos_be" -> """meta:
      |  id: write_bit_query_size_eos_be
      |  bit-endian: be
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: trailing
      |    size-eos: true
      |""".stripMargin,
    "write_bit_query_size_eos_le" -> """meta:
      |  id: write_bit_query_size_eos_le
      |  bit-endian: le
      |seq:
      |  - id: flag
      |    type: b1
      |  - id: trailing
      |    size-eos: true
      |""".stripMargin,
    "write_bit_query_child" -> """meta:
      |  id: write_bit_query_child
      |seq:
      |  - id: child
      |    type: body
      |    size: 2
      |types:
      |  body:
      |    seq:
      |      - id: flag
      |        type: b1
      |      - id: trailing
      |        type: u1
      |        if: not _io.eof
      |""".stripMargin,
    "write_until_index" -> """meta:
      |  id: write_until_index
      |seq:
      |  - id: values
      |    type: u1
      |    repeat: until
      |    repeat-until: _index == 1
      |""".stripMargin,
    "write_until_io" -> """meta:
      |  id: write_until_io
      |seq:
      |  - id: values
      |    type: u1
      |    repeat: until
      |    repeat-until: _io.pos == 2
      |""".stripMargin,
    "write_endian" -> """meta:
      |  id: write_endian
      |  endian:
      |    switch-on: mode
      |    cases:
      |      0: be
      |      1: le
      |params:
      |  - id: mode
      |    type: u1
      |seq:
      |  - id: value
      |    type: u2
      |  - id: unused_child
      |    type: inherited
      |    if: false
      |types:
      |  inherited:
      |    seq:
      |      - id: value
      |        type: u2
      |""".stripMargin
  )

  for ((name, schema) <- writeFixtures) {
    test(s"$name emits field writes without deferred primitive or EOF operations") {
      val code = compileContents(schema)
      for (operation <- Seq("field writing", "padded byte writing", "end-of-stream checking", "end-of-stream condition"))
        assert(!code.contains(s"$operation is not implemented"), s"$name still emits a $operation stub")
      assert(code.contains(".AlignToByte()"), s"$name must finish pending bits")
      val owner = io.kaitai.struct.Utils.upperCamelCase(name)
      val write = method(code, owner, "WriteNoAlign_", "\\([^\\n]*\\)" + errorResult)
      assert(!write.contains("this._io"), "Write expressions must use the output stream")
    }
  }

  // Runtime conformance can be run against a development runtime without making
  // the compiler's ordinary Scala suite depend on a Go installation or network.
  private val instanceFixtures = Seq("write_instances", "write_instance_timing", "write_instance_nested", "write_instance_switch", "write_instance_unpositioned", "write_instance_io", "write_instance_nested_repeat", "write_instance_parameter", "write_instance_child_reference", "write_instance_io_values", "write_instance_io_nested_values", "write_instance_io_conditional")
  for (name <- instanceFixtures) {
    test(s"$name generates positioned instance writers") {
      val code = compile(name)
      assert(!code.contains("positioned instance writing is not implemented"))
      assert(code.contains("Enabled_(v bool)"))
      assert(code.contains("_writeGet"))
    }
  }
  sys.env.get("KAITAI_GO_RUNTIME").foreach { runtimePath =>
    test("generated positioned instances preserve lazy state, exact bytes and seek errors") {
      val dir = Files.createTempDirectory("kaitai-go-instances-")
      def write(name: String, text: String): Unit =
        Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
      val runtime = new File(runtimePath).getCanonicalPath
      write("go.mod", s"module contract\n\ngo 1.24.0\n\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
      for (name <- instanceFixtures)
        write(s"$name.go", compile(name))
      val tests = scala.io.Source.fromURL(getClass.getResource("/go-read-write/instances_test.go"))
      try write("instances_test.go", tests.mkString) finally tests.close()
      val output = new StringBuilder
      def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
      val logger = ProcessLogger(log, log)
      assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
      assert(Process(Seq("go", "test", "-mod=mod", "-v", "./..."), dir.toFile).!(logger) == 0,
        s"Generated Go in $dir failed:\n$output")
      info(s"Generated Go in $dir:\n$output")
    }
    test("failed instance operations cache only rejected materialized values") {
      val dir = Files.createTempDirectory("kaitai-go-instance-validation-")
      def write(name: String, text: String): Unit =
        Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
      val runtime = new File(runtimePath).getCanonicalPath
      write("go.mod", s"module contract\n\ngo 1.24.0\n\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
      Seq(
        "instance_state_scalar" -> """meta:
          |  id: instance_state_scalar
          |instances:
          |  inst:
          |    pos: 0
          |    type: u1
          |    valid: 80
          |""".stripMargin,
        "instance_state_expr" -> """meta:
          |  id: instance_state_expr
          |instances:
          |  items:
          |    pos: 0
          |    type: u1
          |    repeat: expr
          |    repeat-expr: 3
          |    valid:
          |      max: 255
          |""".stripMargin,
        "instance_state_eos" -> """meta:
          |  id: instance_state_eos
          |instances:
          |  items:
          |    pos: 0
          |    type: item
          |    repeat: eos
          |    valid:
          |      expr: _.value >= 0
          |types:
          |  item:
          |    seq:
          |      - id: value
          |        type: u2be
          |""".stripMargin,
        "instance_state_until" -> """meta:
          |  id: instance_state_until
          |instances:
          |  items:
          |    pos: 0
          |    type: u1
          |    repeat: until
          |    repeat-until: stop == 1
          |    valid:
          |      max: 255
          |  stop:
          |    pos: 2
          |    type: u1
          |""".stripMargin,
        "instance_state_dependency" -> """meta:
          |  id: instance_state_dependency
          |instances:
          |  value:
          |    pos: 0
          |    type: u1
          |    valid:
          |      expr: gate == 1
          |  gate:
          |    pos: 1
          |    type: u1
          |""".stripMargin,
        "instance_state_processor" -> """meta:
          |  id: instance_state_processor
          |instances:
          |  value:
          |    pos: 0
          |    size: 1
          |    process: failing_codec(0)
          |    valid:
          |      expr: _.size == 1
          |""".stripMargin,
        "instance_state_conditional" -> """meta:
          |  id: instance_state_conditional
          |instances:
          |  maybe:
          |    pos: 0
          |    type: u1
          |    if: false
          |    valid: 1
          |""".stripMargin,
        "instance_state_empty" -> """meta:
          |  id: instance_state_empty
          |instances:
          |  items:
          |    pos: 0
          |    type: u1
          |    repeat: expr
          |    repeat-expr: 0
          |    valid: 1
          |""".stripMargin
      ).foreach { case (name, schema) => write(s"$name.go", compileContents(schema)) }
      write("instance_validation_state_test.go", instanceValidationStateGoTests)
      val output = new StringBuilder
      def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
      val logger = ProcessLogger(log, log)
      assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
      assert(Process(Seq("go", "test", "-mod=mod", "-count=1", "-v", "./..."), dir.toFile).!(logger) == 0,
        s"Generated Go in $dir failed:\n$output")
      info(s"Generated Go in $dir:\n$output")
    }
    test("generated nested and processed writers preserve bytes and failures") {
      val dir = Files.createTempDirectory("kaitai-go-children-")
      def write(name: String, text: String): Unit =
        Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
      val runtime = new File(runtimePath).getCanonicalPath
      write("go.mod", s"module contract\n\ngo 1.24.0\n\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
      nestedFixtures.foreach(name => write(s"$name.go", compile(name)))
      write("params_pass_array_io.go", compileFile(new File("../tests/formats/params_pass_array_io.ksy"), readWrite = true))
      for (name <- Seq("nested_test.go", "nested_external_test.go")) {
        val tests = scala.io.Source.fromURL(getClass.getResource(s"/go-read-write/$name"))
        try write(name, tests.mkString) finally tests.close()
      }
      val output = new StringBuilder
      def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
      val logger = ProcessLogger(log, log)
      assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
      assert(Process(Seq("go", "test", "-mod=mod", "-v", "./..."), dir.toFile).!(logger) == 0,
        s"Generated Go in $dir failed:\n$output")
      info(s"Generated Go in $dir:\n$output")
    }
    test("generated field writers serialize exact bytes and preserve error causes") {
      val dir = Files.createTempDirectory("kaitai-go-writes-")
      val runtime = new File(runtimePath).getCanonicalPath
      def write(name: String, text: String): Unit =
        Files.write(dir.resolve(name), text.getBytes(StandardCharsets.UTF_8))
      write("go.mod", s"module contract\n\ngo 1.24.0\n\nrequire github.com/kaitai-io/kaitai_struct_go_runtime v0.0.0\nreplace github.com/kaitai-io/kaitai_struct_go_runtime => $runtime\n")
      writeFixtures.foreach { case (name, schema) => write(s"$name.go", compileContents(schema)) }
      write("write_test.go", fieldWriterGoTests)
      val output = new StringBuilder
      def log(line: String): Unit = output.synchronized { output.append(line).append('\n'); () }
      val logger = ProcessLogger(log, log)
      assert(Process(Seq("gofmt", "-w", "."), dir.toFile).!(logger) == 0, output.toString)
      assert(Process(Seq("go", "test", "-mod=mod", "-v", "./..."), dir.toFile).!(logger) == 0,
        s"Generated Go in $dir failed:\n$output")
      info(s"Generated Go in $dir:\n$output")
    }
  }

  private val instanceValidationStateGoTests: String = """package contract
    |import (
    | "bytes"
    | "errors"
    | "testing"
    | "github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
    |)
    |func stream(data []byte) *kaitai.Stream { return kaitai.NewStream(bytes.NewReader(data)) }
    |type failingSeekReader struct { *bytes.Reader }
    |func (r *failingSeekReader) Seek(int64,int)(int64,error) { return 0,errors.New("seek failure") }
    |type FailingCodec struct{}
    |func NewFailingCodec(int64) *FailingCodec { return &FailingCodec{} }
    |func (c *FailingCodec) Decode([]byte)([]byte,error) { return nil,errors.New("codec failure") }
    |func (c *FailingCodec) Encode([]byte)([]byte,error) { return nil,errors.New("codec failure") }
    |func TestRejectedScalarIsRetained(t *testing.T) {
    | v:=NewInstanceStateScalar();if err:=v.Read(stream([]byte{49}),nil,v);err!=nil{t.Fatal(err)}
    | got,err:=v.Inst();var validation kaitai.ValidationNotEqualError
    | if !errors.As(err,&validation)||got!=0||!v._f_inst||v.inst!=49 {t.Fatalf("got=%d err=%v flag=%v stored=%d",got,err,v._f_inst,v.inst)}
    | var consistency kaitai.ConsistencyError
    | if err:=v.Check();!errors.As(err,&consistency)||consistency.ID()!="inst" {t.Fatalf("check: %v",err)}
    | cached,err:=v.Inst();if err!=nil||cached!=49 {t.Fatalf("cached=%d err=%v",cached,err)}
    |}
    |func TestRepeatExprReadFailureRemainsRetryable(t *testing.T) {
    | v:=NewInstanceStateExpr();if err:=v.Read(stream([]byte{1}),nil,v);err!=nil{t.Fatal(err)}
    | if _,err:=v.Items();err==nil||v._f_items||len(v.items)!=1 {t.Fatalf("err=%v flag=%v items=%v",err,v._f_items,v.items)}
    |}
    |func TestRepeatEosNestedFailureRemainsRetryable(t *testing.T) {
    | v:=NewInstanceStateEos();if err:=v.Read(stream([]byte{0,1,2}),nil,v);err!=nil{t.Fatal(err)}
    | if _,err:=v.Items();err==nil||v._f_items||len(v.items)!=1 {t.Fatalf("err=%v flag=%v count=%d",err,v._f_items,len(v.items))}
    |}
    |func TestRepeatUntilPredicateFailureRemainsRetryable(t *testing.T) {
    | v:=NewInstanceStateUntil();if err:=v.Read(stream([]byte{1}),nil,v);err!=nil{t.Fatal(err)}
    | if _,err:=v.Items();err==nil||v._f_items||v._f_stop||len(v.items)!=1 {t.Fatalf("err=%v itemsFlag=%v stopFlag=%v items=%v",err,v._f_items,v._f_stop,v.items)}
    |}
    |func TestValidationDependencyFailureRemainsRetryable(t *testing.T) {
    | v:=NewInstanceStateDependency();if err:=v.Read(stream([]byte{1}),nil,v);err!=nil{t.Fatal(err)}
    | if _,err:=v.Value();err==nil||v._f_value||v._f_gate||v.value!=1 {t.Fatalf("err=%v valueFlag=%v gateFlag=%v value=%d",err,v._f_value,v._f_gate,v.value)}
    |}
    |func TestSeekAndProcessorFailuresRemainRetryable(t *testing.T) {
    | seek:=NewInstanceStateScalar();if err:=seek.Read(kaitai.NewStream(&failingSeekReader{bytes.NewReader([]byte{49})}),nil,seek);err!=nil{t.Fatal(err)}
    | if _,err:=seek.Inst();err==nil||seek._f_inst {t.Fatalf("seek err=%v flag=%v",err,seek._f_inst)}
    | processor:=NewInstanceStateProcessor();if err:=processor.Read(stream([]byte{1}),nil,processor);err!=nil{t.Fatal(err)}
    | if _,err:=processor.Value();err==nil||processor._f_value {t.Fatalf("processor err=%v flag=%v",err,processor._f_value)}
    |}
    |func TestNormalEmptyAndConditionalInstancesCache(t *testing.T) {
    | conditional:=NewInstanceStateConditional();if err:=conditional.Read(stream(nil),nil,conditional);err!=nil{t.Fatal(err)}
    | if got,err:=conditional.Maybe();err!=nil||got!=0||!conditional._f_maybe {t.Fatalf("conditional got=%d err=%v flag=%v",got,err,conditional._f_maybe)}
    | empty:=NewInstanceStateEmpty();if err:=empty.Read(stream(nil),nil,empty);err!=nil{t.Fatal(err)}
    | if got,err:=empty.Items();err!=nil||len(got)!=0||!empty._f_items {t.Fatalf("empty=%v err=%v flag=%v",got,err,empty._f_items)}
    | disabled:=NewInstanceStateScalar();disabled.SetInstEnabled_(false)
    | if disabled._f_inst {t.Fatal("disabling must not materialize instance")};if err:=disabled.Check();err!=nil {t.Fatalf("disabled check: %v",err)}
    |}
    |""".stripMargin

  private val nestedFixtures = Seq("write_nested", "write_processed", "write_custom", "write_processed_child", "write_zlib_child", "write_child_formats", "write_io_param", "write_switch_child", "write_position_child", "write_optional_child", "write_optional_included", "write_optional_peek", "write_optional_included_peek", "write_user_switch", "write_nested_bits_be", "write_nested_bits_le", "write_repeat_bits_be", "write_repeat_bits_le", "write_sized_bits_be", "write_sized_bits_le")
  for (name <- nestedFixtures) {
    test(s"$name supports nested and reverse processing generation") {
      val code = compile(name)
      for (feature <- Seq("nested type writing", "reverse processing", "child writer"))
        assert(!code.contains(s"$feature is not implemented"), s"$name contains deferred $feature")
    }
  }

  private def fieldWriterGoTests: String = """package contract
    |import (
    | "bytes"
    | "errors"
    | "io"
    | "strings"
    | "testing"
    | "github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
    |)
    |type writable interface { Check() error; Write(*kaitai.Writer) error }
    |func exact(t *testing.T, v writable, want []byte) {
    | t.Helper()
    | if err := v.Check(); err != nil { t.Fatal(err) }
    | b := kaitai.NewFixedSeekableBuffer(len(want))
    | if err := v.Write(kaitai.NewWriter(b)); err != nil { t.Fatal(err) }
    | if !bytes.Equal(b.Bytes(), want) { t.Fatalf("got %x, want %x", b.Bytes(), want) }
    | p, _ := b.Seek(0, io.SeekCurrent); if p != int64(len(want)) { t.Fatalf("position %d", p) }
    | successful := &faultBuffer{SeekableBuffer:kaitai.NewFixedSeekableBuffer(len(want))}
    | if err:=v.Write(kaitai.NewWriter(successful));err!=nil {t.Fatal(err)}
    | for n:=1;n<=successful.calls;n++ {for _,short:=range []bool{false,true} {
    |  w:=&faultBuffer{SeekableBuffer:kaitai.NewFixedSeekableBuffer(len(want)),fail:n,short:short}
    |  err:=v.Write(kaitai.NewWriter(w));cause:=injected;if short {cause=io.ErrShortWrite}
    |  if !errors.Is(err,cause)||w.calls!=n {t.Fatalf("%T write %d: %v, calls %d",v,n,err,w.calls)}
    | }}
    | for n:=1;n<=successful.seeks;n++ {
    |  w:=&faultBuffer{SeekableBuffer:kaitai.NewFixedSeekableBuffer(len(want)),failSeek:n}
    |  if err:=v.Write(kaitai.NewWriter(w));!errors.Is(err,injected) {t.Fatalf("%T seek %d: %v",v,n,err)}
    | }
    |}
    |func TestScalars(t *testing.T) {
    | v := NewWriteScalars()
    | v.SetVU1(0xab); v.SetVS1(-2)
    | v.SetVU2be(0x1234); v.SetVU4be(0x12345678); v.SetVU8be(0x123456789abcdef0)
    | v.SetVS2be(-2); v.SetVS4be(-2); v.SetVS8be(-2)
    | v.SetVU2le(0x1234); v.SetVU4le(0x12345678); v.SetVU8le(0x123456789abcdef0)
    | v.SetVS2le(-2); v.SetVS4le(-2); v.SetVS8le(-2)
    | v.SetVF4be(1); v.SetVF8be(1); v.SetVF4le(1); v.SetVF8le(1)
    | exact(t,v,[]byte{0xab,0xfe, 0x12,0x34, 0x12,0x34,0x56,0x78, 0x12,0x34,0x56,0x78,0x9a,0xbc,0xde,0xf0,
    | 0xff,0xfe, 0xff,0xff,0xff,0xfe, 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xfe,
    | 0x34,0x12, 0x78,0x56,0x34,0x12, 0xf0,0xde,0xbc,0x9a,0x78,0x56,0x34,0x12,
    | 0xfe,0xff, 0xfe,0xff,0xff,0xff, 0xfe,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
    | 0x3f,0x80,0,0, 0x3f,0xf0,0,0,0,0,0,0, 0,0,0x80,0x3f, 0,0,0,0,0,0,0xf0,0x3f})
    |}
    |func TestBytes(t *testing.T) {
    | v:=NewWriteBytes(); v.SetFixed([]byte{1,2}); v.SetPadded([]byte{3}); v.SetTerminated([]byte{4})
    | v.SetIncluded([]byte{5,0}); v.SetPeeked([]byte{6}); v.SetMarker(0)
    | v.SetPaddedTerm([]byte{7}); v.SetPaddedIncluded([]byte{8,0}); v.SetTail([]byte{9,10})
    | want:=[]byte{1,2,3,255,255,255,4,0,5,0,6,0,7,0,255,255,8,0,0,0,9,10}
    | exact(t,v,want)
    | parsed:=NewWriteBytes(); if err:=parsed.Read(kaitai.NewStream(bytes.NewReader(want)),nil,parsed);err!=nil {t.Fatal(err)}
    | if !bytes.Equal(parsed.GetPeeked(), []byte{6}) || !bytes.Equal(parsed.GetTail(),[]byte{9,10}) {t.Fatal("round trip")}
    |}
    |func TestStrings(t *testing.T) {
    | v:=NewWriteStrings(); v.SetLatin("é");v.SetUtf("é");v.SetWide("é")
    | exact(t,v,[]byte{0xe9,0xc3,0xa9,0xe9,0})
    |}
    |func TestASCII(t *testing.T) {
    | v:=NewWriteAscii();v.SetPrefix(0x42);v.SetSuffix(0x99);v.SetText("AZ")
    | exact(t,v,[]byte{0x42,0x41,0x5a,0x99})
    | v.SetText("\x00\x7f");exact(t,v,[]byte{0x42,0,0x7f,0x99})
    | checkEncodingError:=func(err error,want rune) {
    |  t.Helper();var cause kaitai.ConsistencyError
    |  if !errors.As(err,&cause)||cause.ID()!="ASCII"||cause.Actual()!=want||errors.Unwrap(err)==nil {t.Fatalf("expected inspectable ASCII error for %U, got %v",want,err)}
    |  if !strings.Contains(err.Error(),"text") {t.Fatalf("missing field context: %v",err)}
    | }
    | for _,tc:=range []struct{text string;bad rune}{{"é",'é'},{"Aé",'é'},{"界",'界'},{string([]byte{0xff}), '\ufffd'}} {
    |  v.SetText(tc.text);checkEncodingError(v.Check(),tc.bad)
    | }
    | // The reader historically accepts arbitrary ASCII bytes and marks the
    | // object checked. Write must still validate before writing this field.
    | parsed:=NewWriteAscii()
    | if err:=parsed.Read(kaitai.NewStream(bytes.NewReader([]byte{0x42,0xc3,0xa9,0x99})),nil,parsed);err!=nil {t.Fatal(err)}
    | out:=&bytes.Buffer{};checkEncodingError(parsed.Write(kaitai.NewWriter(out)),'é')
    | if !bytes.Equal(out.Bytes(),[]byte{0x42}) {t.Fatalf("affected field or following data written: %x",out.Bytes())}
    |}
    |func TestControl(t *testing.T) {
    | v:=NewWriteControl(); v.SetTag(WriteControl_Tags__One); v.SetConditional(0x1234);v.SetSwitched(0x5678);v.SetIntegerSwitch(9)
    | v.SetFixed([]uint8{1,2});v.SetUntil([]uint8{3,0});v.SetEos([]uint8{4,5})
    | exact(t,v,[]byte{1,0x34,0x12,0x78,0x56,9,1,2,3,0,4,5})
    | v.SetTag(WriteControl_Tags__Two);v.SetSwitched(0x12345678);v.SetIntegerSwitch(0x1234)
    | exact(t,v,[]byte{2,0x78,0x56,0x34,0x12,0x34,0x12,1,2,3,0,4,5})
    | if err:=v.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(12)));err==nil {t.Fatal("expected early EOF inconsistency")}
    | if err:=v.Write(kaitai.NewWriter(kaitai.NewFixedSeekableBuffer(14)));err==nil {t.Fatal("expected missing EOF inconsistency")}
    |}
    |func TestMixedSwitch(t *testing.T) {
    | v:=NewWriteMixedSwitch();v.SetTag(1);v.SetValue(uint8(9));exact(t,v,[]byte{1,9})
    | v.SetTag(2);v.SetValue([]byte{7,8});exact(t,v,[]byte{2,7,8})
    |}
    |func TestBits(t *testing.T) {
    | be:=NewWriteBitsBe();be.SetFlag(true);be.SetBits(5);be.SetMarker(0xab);be.SetTail(3)
    | exact(t,be,[]byte{0xd0,0xab,0x60})
    | le:=NewWriteBitsLe();le.SetFlag(true);le.SetBits(5);le.SetMarker(0xab);le.SetTail(3)
    | exact(t,le,[]byte{0x0b,0xab,3})
    |}
    |func TestWriterSize(t *testing.T) {
    | v:=NewWriteSized();v.SetData([]byte{1,2,3});exact(t,v,[]byte{1,2,3})
    | root:=NewWriteRootSized();if err:=root.Read(kaitai.NewStream(bytes.NewReader([]byte{1,2})),nil,root);err!=nil {t.Fatal(err)}
    | root.SetData([]byte{3,4,5});exact(t,root,[]byte{3,4,5})
    |}
    |func TestBitQueries(t *testing.T) {
    | v:=NewWriteBitContext();v.SetPrefix(5);v.SetSuffix(3);exact(t,v,[]byte{0xa3})
    | bits:=NewWriteBitEos();bits.SetValues([]bool{true,false,true,false,false,false,true,true});exact(t,bits,[]byte{0xa3})
    | nibbles:=NewWriteBitEosB4();nibbles.SetValues([]uint64{0xa,0xb});exact(t,nibbles,[]byte{0xab})
    | boundary:=NewWriteBitEofBoundary();boundary.SetFoo(0x142f);boundary.SetBar(0xf);exact(t,boundary,[]byte{0x01,0x42,0xff})
    |}
    |func writeBitQuery(t *testing.T,v writable,out *kaitai.SeekableBuffer,want []byte) {
    | t.Helper()
    | if err:=v.Check();err!=nil {t.Fatal(err)}
    | if err:=v.Write(kaitai.NewWriter(out));err!=nil {t.Fatal(err)}
    | if !bytes.Equal(out.Bytes(),want) {t.Fatalf("got %x, want %x",out.Bytes(),want)}
    |}
    |func TestPartialBitQueriesUseLogicalPositionAndExtent(t *testing.T) {
    | for _,fixed:=range []bool{false,true} {
    |  makeOut:=func(size int)*kaitai.SeekableBuffer {if fixed{return kaitai.NewFixedSeekableBuffer(size)};return kaitai.NewSeekableBuffer(nil)}
    |  eofBe:=NewWriteBitQueryEofBe();eofBe.SetFlag(true);eofBe.SetTrailing(0x11);writeBitQuery(t,eofBe,makeOut(2),[]byte{0x80,0x11})
    |  eofLe:=NewWriteBitQueryEofLe();eofLe.SetFlag(true);eofLe.SetTrailing(0x11);writeBitQuery(t,eofLe,makeOut(2),[]byte{0x01,0x11})
    |  posBe:=NewWriteBitQueryPosBe();posBe.SetFlag(true);posBe.SetTrailing(0x11);writeBitQuery(t,posBe,makeOut(2),[]byte{0x80,0x11})
    |  posLe:=NewWriteBitQueryPosLe();posLe.SetFlag(true);posLe.SetTrailing(0x11);writeBitQuery(t,posLe,makeOut(2),[]byte{0x01,0x11})
    |  expectedSize:=uint8(1);if fixed {expectedSize=2}
    |  sizeBe:=NewWriteBitQuerySizeBe(expectedSize);sizeBe.SetFlag(true);sizeBe.SetTrailing(0x11);writeBitQuery(t,sizeBe,makeOut(2),[]byte{0x80,0x11})
    |  sizeLe:=NewWriteBitQuerySizeLe(expectedSize);sizeLe.SetFlag(true);sizeLe.SetTrailing(0x11);writeBitQuery(t,sizeLe,makeOut(2),[]byte{0x01,0x11})
    |  eosBe:=NewWriteBitQuerySizeEosBe();eosBe.SetFlag(true);eosBe.SetTrailing([]byte{0x11});writeBitQuery(t,eosBe,makeOut(2),[]byte{0x80,0x11})
    |  eosLe:=NewWriteBitQuerySizeEosLe();eosLe.SetFlag(true);eosLe.SetTrailing([]byte{0x11});writeBitQuery(t,eosLe,makeOut(2),[]byte{0x01,0x11})
    | }
    |}
    |func TestPartialBitQueryUsesChildWriterContext(t *testing.T) {
    | v:=NewWriteBitQueryChild()
    | v.SetRoot_(v)
    | child:=NewWriteBitQueryChild_Body();child.SetParent_(v);child.SetRoot_(v);child.SetFlag(true);child.SetTrailing(0x11)
    | v.SetChild(child)
    | if err:=child.Check();err!=nil {t.Fatal(err)}
    | exact(t,v,[]byte{0x80,0x11})
    |}
    |func TestUntilContext(t *testing.T) {
    | a:=NewWriteUntilIndex();a.SetValues([]uint8{1,2});exact(t,a,[]byte{1,2})
    | parsed:=NewWriteUntilIndex();if err:=parsed.Read(kaitai.NewStream(bytes.NewReader([]byte{1,2})),nil,parsed);err!=nil {t.Fatal(err)}
    | if len(parsed.GetValues())!=2 {t.Fatal("repeat-until _index must be zero-based")}
    | b:=NewWriteUntilIo();b.SetValues([]uint8{3,4});exact(t,b,[]byte{3,4})
    |}
    |func TestOptionalTerminator(t *testing.T) {
    | v:=NewWriteEosTerm();v.SetData([]byte{1,2});exact(t,v,[]byte{1,2});exact(t,v,[]byte{1,2,0})
    | included:=NewWriteIncludedOptional();included.SetData([]byte{1,2});exact(t,included,[]byte{1,2})
    | included.SetData([]byte{1,2,0});exact(t,included,[]byte{1,2,0})
    | padded:=NewWriteEosPadding();padded.SetHeader(1);padded.SetData([]byte{2});exact(t,padded,[]byte{1,2,255,255})
    |}
    |func TestEndian(t *testing.T) {
    | for mode:=uint8(0); mode<2; mode++ {
    | v:=NewWriteEndian(mode);v.SetValue(0x1234)
    | want:=[]byte{0x12,0x34};if mode==1 {want=[]byte{0x34,0x12}}
    | exact(t,v,want)
    | child:=NewWriteEndian_Inherited()
    | if err:=child.Read(kaitai.NewStream(bytes.NewReader(want)),v,v);err!=nil {t.Fatal(err)}
    | child.SetValue(0x1234);exact(t,child,want)
    | }
    |}
    |var injected=errors.New("injected output error")
    |type failingWriter struct { calls, fail int; short bool }
    |func (w *failingWriter) Write(p []byte)(int,error) { w.calls++;if w.calls==w.fail {if w.short {return 0,nil};return 0,injected};return len(p),nil }
    |type failingSeeker struct { *kaitai.SeekableBuffer; calls, fail int }
    |func (w *failingSeeker) Seek(off int64,whence int)(int64,error) {w.calls++;if w.calls==w.fail{return 0,injected};return w.SeekableBuffer.Seek(off,whence)}
    |type faultBuffer struct { *kaitai.SeekableBuffer; calls,fail,seeks,failSeek int;short bool }
    |func (w *faultBuffer) Write(p []byte)(int,error) {w.calls++;if w.calls==w.fail {if w.short{return 0,nil};return 0,injected};return w.SeekableBuffer.Write(p)}
    |func (w *faultBuffer) Seek(off int64,whence int)(int64,error) {w.seeks++;if w.seeks==w.failSeek{return 0,injected};return w.SeekableBuffer.Seek(off,whence)}
    |func TestErrors(t *testing.T) {
    | v:=NewWriteScalars();if err:=v.Check();err!=nil{t.Fatal(err)}
    | for n:=1;n<=18;n++ {for _,short:=range []bool{false,true} {w:=&failingWriter{fail:n,short:short};err:=v.Write(kaitai.NewWriter(w));cause:=injected;if short {cause=io.ErrShortWrite};if !errors.Is(err,cause)||w.calls!=n {t.Fatalf("call %d: %v, calls %d",n,err,w.calls)}}}
    | bits:=NewWriteBitsBe();bits.SetFlag(true);bits.SetBits(5);bits.SetMarker(1);bits.SetTail(3);if err:=bits.Check();err!=nil{t.Fatal(err)}
    | for n:=1;n<=3;n++ {w:=&failingWriter{fail:n};if err:=bits.Write(kaitai.NewWriter(w));!errors.Is(err,injected)||w.calls!=n {t.Fatalf("bit call %d: %v",n,err)}}
    | sized:=NewWriteSized();sized.SetData([]byte{1});if err:=sized.Check();err!=nil{t.Fatal(err)}
    | for n:=1;n<=3;n++ {w:=&failingSeeker{SeekableBuffer:kaitai.NewFixedSeekableBuffer(1),fail:n};if err:=sized.Write(kaitai.NewWriter(w));!errors.Is(err,injected){t.Fatalf("seek %d: %v",n,err)}}
    | nonseek:=&bytes.Buffer{};if err:=sized.Write(kaitai.NewWriter(nonseek));!errors.Is(err,kaitai.ErrWriterNotSeekable)||nonseek.Len()!=0 {t.Fatalf("nonseekable: %v",err)}
    |}
    |""".stripMargin

  private def accessor(code: String, owner: String, field: String, goType: String): Unit = {
    method(code, owner, s"Get$field", s"\\(\\s*\\)\\s+${quote(goType)}")
    method(code, owner, s"Set$field", s"\\(\\s*\\w+\\s+${quote(goType)}\\s*\\)")
  }

  test("read-only output retains exported fields and has no serialization lifecycle") {
    val code = compile("go_rw_simple", readWrite = false)
    contains(code, "(?m)^\\s+Value\\s+uint32\\s*$", "exported Value field")
    contains(code, "(?m)^\\s+Payload\\s+\\[\\]byte\\s*$", "exported Payload field")
    assert(!"(?m)^func\\s+\\([^\\n]+\\)\\s+(?:GetValue|SetValue|GetPayload|SetPayload|Check|Write)\\(".r
      .findFirstIn(code).isDefined, "Read-only output must not expose serialization methods")
  }

  test("read-write sequence storage is private") {
    val code = compile("go_rw_simple")
    contains(code, "(?m)^\\s+[a-z_][A-Za-z0-9_]*\\s+uint32\\s*$", "private uint32 sequence storage")
    contains(code, "(?m)^\\s+[a-z_][A-Za-z0-9_]*\\s+\\[\\]byte\\s*$", "private byte sequence storage")
    assert(!"(?m)^\\s+(?:Value|Payload)\\s".r.findFirstIn(code).isDefined,
      "Exported sequence fields bypass setter invalidation")
  }

  test("read-write scalar and byte fields expose typed getters and setters") {
    val code = compile("go_rw_simple")
    accessor(code, "GoRwSimple", "Value", "uint32")
    accessor(code, "GoRwSimple", "Payload", "[]byte")
  }

  test("setters invalidate the consistency state restored by Check") {
    val code = compile("go_rw_simple")
    val setter = method(code, "GoRwSimple", "SetValue", "\\(\\s*\\w+\\s+uint32\\s*\\)")
    val state = "\\b\\w+\\.([a-z_][A-Za-z0-9_]*)\\s*=\\s*(true|false)\\b".r
      .findFirstMatchIn(setter).getOrElse(fail("Missing generated behavior: setter invalidates consistency state"))
    val check = method(code, "GoRwSimple", "Check", "\\(\\s*\\)" + errorResult)
    val clean = if (state.group(2) == "true") "false" else "true"
    contains(check, s"\\b\\w+\\.${quote(state.group(1))}\\s*=\\s*$clean\\b", "Check restores the same consistency state")
  }

  test("Write accepts the separate Writer and rejects unchecked objects") {
    val code = compile("go_rw_simple")
    val write = method(code, "GoRwSimple", "Write", "\\(\\s*\\w+\\s+\\*kaitai\\.Writer\\s*\\)" + errorResult)
    contains(write, "WriteNoAlign_", "Write delegates to the checked serialization core")
    val core = method(code, "GoRwSimple", "WriteNoAlign_", "\\(\\s*\\w+\\s+\\*kaitai\\.Writer\\s*\\)" + errorResult)
    contains(core, "ConsistencyNotCheckedError", "serialization core consistency guard")
  }

  test("repeated fields expose slice accessors") {
    accessor(compile("go_rw_repeated"), "GoRwRepeated", "Values", "[]uint16")
  }

  test("sized nested types expose pointer accessors and their own lifecycle") {
    val code = compile("go_rw_nested")
    accessor(code, "GoRwNested", "Child", "*GoRwNested_ChildType")
    accessor(code, "GoRwNested_ChildType", "Value", "uint16")
    method(code, "GoRwNested_ChildType", "Check", "\\(\\s*\\)" + errorResult)
    method(code, "GoRwNested_ChildType", "Write", "\\(\\s*\\w+\\s+\\*kaitai\\.Writer\\s*\\)" + errorResult)
  }

  test("value instances retain lazy access and positioned instances support assignment") {
    val code = compile("go_rw_instances")
    method(code, "GoRwInstances", "Doubled", "\\(\\s*\\)\\s+\\(\\s*\\w+\\s+int\\s*,\\s*\\w+\\s+error\\s*\\)")
    method(code, "GoRwInstances", "Marker", "\\(\\s*\\)\\s+\\(\\s*\\w+\\s+uint16\\s*,\\s*\\w+\\s+error\\s*\\)")
    method(code, "GoRwInstances", "SetMarker", "\\(\\s*\\w+\\s+uint16\\s*\\)")
  }

  test("bit fields expose boolean and unsigned accessors") {
    val code = compile("go_rw_bits")
    accessor(code, "GoRwBits", "Flag", "bool")
    accessor(code, "GoRwBits", "Count", "uint64")
    method(code, "GoRwBits", "Write", "\\(\\s*\\w+\\s+\\*kaitai\\.Writer\\s*\\)" + errorResult)
  }

  test("length and repeat consistency checks carry expected and actual values without IO") {
    for ((fixture, owner, field, expected) <- Seq(
      ("go_rw_simple", "GoRwSimple", "payload", 3),
      ("go_rw_repeated", "GoRwRepeated", "values", 2)
    )) {
      val check = method(compile(fixture), owner, "Check", "\\(\\s*\\)" + errorResult)
      contains(check, s"NewConsistencyError\\(\"$field\",\\s*$expected,\\s*len\\(", "typed length error with field context")
      assert(!check.contains("._io"), "Independent checks must not touch the read stream")
      assert(check.indexOf("_checked = false") < check.indexOf("NewConsistencyError"))
      assert(check.indexOf("NewConsistencyError") < check.indexOf("_checked = true"))
      assert(!check.contains("not implemented"))
    }
  }

  test("fixed contents and explicit validations become typed consistency failures") {
    val code = compileContents(
      """meta:
        |  id: go_rw_validated
        |seq:
        |  - id: magic
        |    contents: [75, 83]
        |  - id: version
        |    type: u1
        |    valid: 1
        |  - id: values
        |    type: u1
        |    repeat: expr
        |    repeat-expr: 2
        |    valid:
        |      max: 9
        |""".stripMargin)
    val check = method(code, "GoRwValidated", "Check", "\\(\\s*\\)" + errorResult)
    for (field <- Seq("magic", "version", "values")) {
      contains(check, s"NewConsistencyError\\(\"$field\",", s"typed $field consistency error")
    }
    contains(check, "bytes\\.Equal", "fixed contents comparison")
    assert(!check.contains("._io"))
    assert(!check.contains("NewValidation"), "Check must report consistency errors, while Read retains validation errors")
    contains(code, "NewValidationNotEqualError", "read validation remains available")
  }

  test("failed parse-instance validation retains a materialized value for Check") {
    val code = compileContents(
      """meta:
        |  id: go_rw_failed_instance
        |instances:
        |  inst:
        |    pos: 0
        |    type: u1
        |    valid: 80
        |""".stripMargin)
    val instance = method(code, "GoRwFailedInstance", "Inst", "\\(\\s*\\)\\s*\\([^\\n]*\\)")
    contains(
      instance,
      "(?s)ReadU1\\(\\).*?if err != nil.*?this\\.inst = tmp\\d+\\s+if .*?\\{\\s+this\\._f_inst = true\\s+return .*?NewValidationNotEqualError",
      "retryable read followed by a retained rejected parse instance"
    )
  }

  test("nested parent and root consistency use identity checks") {
    val check = method(compile("go_rw_nested"), "GoRwNested", "Check", "\\(\\s*\\)" + errorResult)
    contains(check, "NewConsistencyError\\(\"child\",", "nested context error")
    contains(check, "\\._root\\s*!=\\s*this\\._root", "root identity check")
    contains(check, "\\._parent\\s*!=\\s*this", "parent identity check")
    assert(!check.contains("not implemented"))
    assert(!check.contains("._io"))
  }

  test("missing nested values fail consistency before dereferencing their metadata") {
    val check = method(compile("go_rw_nested"), "GoRwNested", "Check", "\\(\\s*\\)" + errorResult)
    contains(check, "if this\\._m_Child == nil", "nil nested value check")
    assert(check.indexOf("== nil") < check.indexOf("._root"))
  }

  test("IO-dependent forced parent expressions are deferred without evaluating the IO") {
    val code = compileContents(
      """meta:
        |  id: go_rw_parent_io
        |seq:
        |  - id: child
        |    type: child_type
        |    parent: '_io.pos == 0 ? _root : _root'
        |types:
        |  child_type:
        |    seq:
        |      - id: value
        |        type: u1
        |""".stripMargin)
    val check = method(code, "GoRwParentIo", "Check", "\\(\\s*\\)" + errorResult)
    assert(!check.contains("._io"), "Skipping an IO-dependent parent check must also skip expression evaluation")
  }

  test("every mutable setter dirties state and successful Read cleans only at the end") {
    for ((fixture, owner, fields) <- Seq(
      ("go_rw_simple", "GoRwSimple", Seq("Value" -> "uint32", "Payload" -> "[]byte")),
      ("go_rw_repeated", "GoRwRepeated", Seq("Values" -> "[]uint16")),
      ("go_rw_bits", "GoRwBits", Seq("Flag" -> "bool", "Count" -> "uint64")),
      ("go_rw_nested", "GoRwNested", Seq("Child" -> "*GoRwNested_ChildType"))
    )) {
      val code = compile(fixture)
      for ((field, goType) <- fields) {
        val setter = method(code, owner, s"Set$field", s"\\(\\s*\\w+\\s+${quote(goType)}\\s*\\)")
        contains(setter, "_checked\\s*=\\s*false", s"$field setter invalidation")
      }
      val read = method(code, owner, "Read", "\\([^\\n]*\\)" + errorResult)
      assert(read.indexOf("_checked = false") < read.indexOf("this._io = io"))
      assert(read.lastIndexOf("_checked = err == nil") > read.lastIndexOf("if err != nil"))
    }
  }

  test("slice accessors explain how to recover from in-place mutation") {
    val code = compile("go_rw_simple")
    contains(code, "(?s)//[^\\n]*in-place.*Check\\(\\)", "slice mutation limitation and Check recovery")
  }

  test("dynamic lengths widen both operands without narrowing slice length") {
    val code = compileContents(
      """meta:
        |  id: go_rw_dynamic
        |seq:
        |  - id: count
        |    type: u1
        |  - id: payload
        |    size: count
        |""".stripMargin)
    val check = method(code, "GoRwDynamic", "Check", "\\(\\s*\\)" + errorResult)
    contains(check, "uint64\\(len\\(this\\._m_Payload\\)\\)\\s*!=\\s*uint64\\(this\\._m_Count\\)", "non-narrowing dynamic length comparison")
  }

  test("object array parameters compare element identity instead of pointed-to contents") {
    val code = compileContents(
      """meta:
        |  id: go_rw_identity
        |seq:
        |  - id: items
        |    type: item
        |    repeat: expr
        |    repeat-expr: 2
        |  - id: holder
        |    type: holder(items)
        |types:
        |  item:
        |    seq:
        |      - id: value
        |        type: u1
        |  holder:
        |    params:
        |      - id: expected_items
        |        type: item[]
        |""".stripMargin)
    val check = method(code, "GoRwIdentity", "Check", "\\(\\s*\\)" + errorResult)
    assert(!check.contains("reflect.DeepEqual"), "Distinct object pointers must not compare equal through deep reflection")
    contains(check, "a\\[i\\]\\s*==\\s*b\\[i\\]", "identity comparison for object array elements")
  }

  test("dynamic parameter equality preserves pointers through nested collections") {
    val code = compileContents(
      """meta:
        |  id: go_rw_dynamic_identity
        |params:
        |  - id: input
        |    type: any
        |seq:
        |  - id: holder
        |    type: holder(input)
        |types:
        |  holder:
        |    params:
        |      - id: expected
        |        type: any
        |""".stripMargin)
    val check = method(code, "GoRwDynamicIdentity", "Check", "\\(\\s*\\)" + errorResult)
    assert(!check.contains("reflect.DeepEqual"), "Dynamic values must not erase generated-object identity")
    contains(check, "reflect\\.Slice", "recursive collection comparison for dynamic values")
    contains(check, "Comparable\\(\\)", "identity-safe comparison for dynamic pointers")
  }

  test("dynamic parameter checks remember visited slice pairs before following cycles") {
    val code = compileContents(
      """meta:
        |  id: go_rw_cyclic_identity
        |params:
        |  - id: input
        |    type: any
        |seq:
        |  - id: holder
        |    type: holder(input)
        |types:
        |  holder:
        |    params:
        |      - id: expected
        |        type: any
        |""".stripMargin)
    val check = method(code, "GoRwCyclicIdentity", "Check", "\\(\\s*\\)" + errorResult)
    contains(check, "visited\\[pair\\]", "pair memoization for cyclic dynamic collections")
    assert(check.indexOf("visited[pair] = true") < check.indexOf("equal(a.Index(i), b.Index(i))"),
      "Remember a pair before descending into its elements")
  }
}
