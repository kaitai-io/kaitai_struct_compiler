package io.kaitai.struct.datatype

import io.kaitai.struct.datatype.DataType.SwitchType
import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.format.ParseUtils
import io.kaitai.struct.problems.KSYParseError

sealed trait Endianness

sealed abstract class FixedEndian extends Endianness {
  def toSuffix: String
}
case object LittleEndian extends FixedEndian {
  override def toSuffix = "le"
}
case object BigEndian extends FixedEndian {
  override def toSuffix = "be"
}

case class CalcEndian(on: Ast.expr, cases: Map[Ast.expr, FixedEndian]) extends Endianness

case object InheritedEndian extends Endianness

object Endianness {
  def fromYaml(src: Option[Any], path: List[String]): Option[Endianness] = {
    src match {
      case None => None
      case Some("be") => Some(BigEndian)
      case Some("le") => Some(LittleEndian)
      case Some(srcMap: Map[Any, Any]) =>
        val endianMap = ParseUtils.asMapStr(srcMap, path)
        Some(fromMap(endianMap, path))
      case _ =>
        throw KSYParseError.withText(
          s"unable to parse endianness: `le`, `be` or calculated endianness map is expected",
          path ++ List("endian")
        )
    }
  }

  def fromMap(srcMap: Map[String, Any], path: List[String]): CalcEndian = {
    val (_on, _cases) = SwitchType.fromYaml1(srcMap, path)

    val on = Expressions.parse(_on)
    val cases: Map[Ast.expr, FixedEndian] = _cases.map { case (condition, endStr) =>
      Expressions.parse(condition) -> (endStr match {
        case "be" => BigEndian
        case "le" => LittleEndian
        case _ =>
          throw KSYParseError.badDictValue(Set("be", "le"), endStr, path ++ List("cases", condition))
      })
    }

    CalcEndian(on, cases)
  }

  def fromString(s: Option[String], defaultEndian: Option[Endianness], dt: String, path: List[String]): Option[FixedEndian] = s match {
    case Some("le") => Some(LittleEndian)
    case Some("be") => Some(BigEndian)
    case None =>
      defaultEndian match {
        case Some(e: FixedEndian) => Some(e)
        case Some(_: CalcEndian) | Some(InheritedEndian) => None // to be overridden during compile
        case None =>
          throw KSYParseError.withText(s"unable to use type '$dt' without default endianness", path ++ List("type"))
      }
  }
}
