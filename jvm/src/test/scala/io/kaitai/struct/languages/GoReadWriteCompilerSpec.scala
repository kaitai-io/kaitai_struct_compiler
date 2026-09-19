package io.kaitai.struct.languages

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.{Main, RuntimeConfig, Version}
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.regex.Pattern.quote

/** Contract tests for the output of the public parsing/precompilation/generation pipeline.
  * These intentionally use small structural assertions rather than full-file snapshots.
  * These surface checks do not replace runtime conformance tests.
  */
class GoReadWriteCompilerSpec extends AnyFunSuite {
  KSVersion.current = Version.version

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
    contains(write, "ConsistencyNotCheckedError", "Write consistency guard")
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
