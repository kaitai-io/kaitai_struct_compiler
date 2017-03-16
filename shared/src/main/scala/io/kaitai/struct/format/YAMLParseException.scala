package io.kaitai.struct.format

import fastparse.StringReprOps
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.Expressions

class YAMLParseException(val msg: String, val path: List[String])
  extends RuntimeException(s"/${path.mkString("/")}: $msg", null)

object YAMLParseException {
  def badType(expected: String, got: Any, path: List[String]): YAMLParseException = {
    val gotStr = got match {
      case null => "null"
      case _ => s"$got (${got.getClass})"
    }
    new YAMLParseException(s"expected $expected, got $gotStr", path)
  }

  def badDictValue(expected: Set[String], got: String, path: List[String]): YAMLParseException =
    new YAMLParseException(s"expected ${expected.toList.sorted.mkString(" / ")}, got '$got'", path)

  def incompatibleVersion(expected: KSVersion, got: KSVersion, path: List[String]): YAMLParseException =
    new YAMLParseException(
      s"this ksy requires compiler version at least $expected, but you have $got",
      path
    )

  def invalidId(id: String, entity: String, path: List[String]): YAMLParseException =
    new YAMLParseException(
      s"invalid $entity ID: '$id', expected /${Identifier.ReIdentifier.toString}/",
      path
    )

  def expression(epe: Expressions.ParseException, path: List[String]): YAMLParseException = {
    val f = epe.failure
    val pos = StringReprOps.prettyIndex(f.extra.input, f.index)
    new YAMLParseException(
      s"parsing expression '${epe.src}' failed on $pos, expected ${f.extra.traced.expected}",
      path
    )
  }

  def exprType(expected: String, got: DataType, path: List[String]): YAMLParseException =
    new YAMLParseException(s"invalid type: expected $expected, got $got", path)
}
