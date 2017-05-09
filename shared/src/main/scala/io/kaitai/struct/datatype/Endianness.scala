package io.kaitai.struct.datatype

import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.format.{ParseUtils, YAMLParseException}

sealed trait Endianness

abstract class FixedEndian extends Endianness {
  def toSuffix: String
}
case object LittleEndian extends FixedEndian {
  override def toSuffix = "le"
}
case object BigEndian extends FixedEndian {
  override def toSuffix = "be"
}

abstract class CalcEndian extends Endianness
case class CalcEndianOne(isLittle: Ast.expr) extends CalcEndian
case class CalcEndianTwo(isLittle: Ast.expr, isBig: Ast.expr) extends CalcEndian

object Endianness {
  def defaultFromMap(srcMap: Map[String, Any], path: List[String]): Option[Endianness] = {
    Endianness.defaultFromString(
      ParseUtils.getOptValueStr(srcMap, "endian", path),
      ParseUtils.getOptValueStr(srcMap, "endian-is-le", path),
      ParseUtils.getOptValueStr(srcMap, "endian-is-be", path),
      path
    )
  }

  def defaultFromString(s: Option[String], isLittle: Option[String], isBig: Option[String], path: List[String]): Option[Endianness] = s match {
    case None => None
    case Some("be") => Some(BigEndian)
    case Some("le") => Some(LittleEndian)
    case Some("expr") =>
      (isLittle, isBig) match {
        case (None, None) =>
          throw new YAMLParseException(
            s"when using endian: expr, at least one of `endian-is-le` or `endian-is-be` must be defined",
            path ++ List("endian")
          )
        case (Some(le), None) =>
          Some(CalcEndianOne(Expressions.parse(le)))
        case (None, Some(be)) =>
          Some(CalcEndianOne(Ast.expr.UnaryOp(Ast.unaryop.Not, Expressions.parse(be))))
        case (Some(le), Some(be)) =>
          Some(CalcEndianTwo(
            Expressions.parse(le),
            Expressions.parse(be)
          ))
      }
    case Some(unknown) => throw YAMLParseException.badDictValue(
      Set("be", "le"), unknown, path ++ List("endian")
    )
  }

  def fromString(s: Option[String], defaultEndian: Option[Endianness], dt: String, path: List[String]): Option[FixedEndian] = s match {
    case Some("le") => Some(LittleEndian)
    case Some("be") => Some(BigEndian)
    case None =>
      defaultEndian match {
        case Some(e: FixedEndian) => Some(e)
        case Some(_: CalcEndian) => None // to be overridden during compile
        case None =>
          throw new YAMLParseException(s"unable to use type '$dt' without default endianness", path ++ List("type"))
      }
  }
}
