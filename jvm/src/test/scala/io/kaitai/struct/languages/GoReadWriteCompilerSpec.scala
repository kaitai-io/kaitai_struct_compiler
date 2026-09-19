package io.kaitai.struct.languages

import io.kaitai.struct.JavaMain.CLIConfig
import io.kaitai.struct.{Main, RuntimeConfig, Version}
import io.kaitai.struct.format.KSVersion
import io.kaitai.struct.formats.JavaKSYParser
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
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
    val runtime = RuntimeConfig(readWrite = readWrite, goPackage = "contract")
    val (specsOption, problems) = JavaKSYParser.localFileToSpecs(
      new File(resource.toURI).getPath, CLIConfig(runtime = runtime)
    )
    val specs = specsOption.getOrElse(fail(s"Invalid fixture $fixture: ${problems.mkString(", ")}"))
    val files = try {
      Main.compile(specs, specs.firstSpec, GoCompiler, runtime).files
    } catch {
      case e: NotImplementedError if readWrite =>
        fail(s"Missing generated behavior: Go read-write compiler hook for $fixture", e)
    }
    assert(files.size == 1, s"Expected one generated Go file for $fixture")
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
}
