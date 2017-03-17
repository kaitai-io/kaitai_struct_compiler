package io.kaitai.struct.datatype

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

sealed trait DataType

/**
  * A collection of case objects and classes that are used to represent internal
  * Kaitai Struct type system.
  */
object DataType {
  abstract class IntWidth(val width: Int)
  case object Width1 extends IntWidth(1)
  case object Width2 extends IntWidth(2)
  case object Width4 extends IntWidth(4)
  case object Width8 extends IntWidth(8)

  /**
    * A common trait for all types that can be read with a simple,
    * parameterless KaitaiStream API call.
    */
  trait ReadableType extends DataType {
    def apiCall: String
  }

  abstract class NumericType extends DataType
  abstract class BooleanType extends DataType

  abstract class IntType extends NumericType
  case object CalcIntType extends IntType
  case class Int1Type(signed: Boolean) extends IntType with ReadableType {
    override def apiCall: String = if (signed) "s1" else "u1"
  }
  case class IntMultiType(signed: Boolean, width: IntWidth, endian: Endianness) extends IntType with ReadableType {
    override def apiCall: String = {
      val ch1 = if (signed) 's' else 'u'
      s"$ch1${width.width}${endian.toString}"
    }
  }
  case object BitsType1 extends BooleanType
  case class BitsType(width: Int) extends IntType

  abstract class FloatType extends NumericType
  case object CalcFloatType extends FloatType
  case class FloatMultiType(width: IntWidth, endian: Endianness) extends FloatType with ReadableType {
    override def apiCall: String = {
      s"f${width.width}${endian.toString}"
    }
  }

  trait Processing {
    def process: Option[ProcessExpr]
  }

  abstract class BytesType extends DataType with Processing
  case object CalcBytesType extends BytesType {
    override def process = None
  }
  case class FixedBytesType(contents: Array[Byte], override val process: Option[ProcessExpr]) extends BytesType
  case class BytesEosType(
    terminator: Option[Int],
    include: Boolean,
    padRight: Option[Int],
    override val process: Option[ProcessExpr]
  ) extends BytesType
  case class BytesLimitType(
    size: Ast.expr,
    terminator: Option[Int],
    include: Boolean,
    padRight: Option[Int],
    override val process: Option[ProcessExpr]
  ) extends BytesType
  case class BytesTerminatedType(
    terminator: Int,
    include: Boolean,
    consume: Boolean,
    eosError: Boolean,
    override val process: Option[ProcessExpr]
  ) extends BytesType

  abstract class StrType extends DataType
  case object CalcStrType extends StrType
  case class StrFromBytesType(bytes: BytesType, encoding: String) extends StrType

  case object CalcBooleanType extends BooleanType
  case class ArrayType(elType: DataType) extends DataType

  abstract class UserType(val name: List[String], val forcedParent: Option[Ast.expr]) extends DataType {
    var classSpec: Option[ClassSpec] = None
    def isOpaque = {
      val cs = classSpec.get
      cs.isTopLevel || (cs.meta match {
        case None => false
        case Some(meta) => meta.isOpaque
      })
    }
  }
  case class UserTypeInstream(_name: List[String], _forcedParent: Option[Ast.expr]) extends UserType(_name, _forcedParent)
  case class UserTypeFromBytes(
    _name: List[String],
    _forcedParent: Option[Ast.expr],
    bytes: BytesType,
    override val process: Option[ProcessExpr]
  ) extends UserType(_name, _forcedParent) with Processing

  val USER_TYPE_NO_PARENT = Ast.expr.Bool(false)

  case object AnyType extends DataType
  case object KaitaiStructType extends DataType
  case object KaitaiStreamType extends DataType

  case class EnumType(name: List[String], basedOn: IntType) extends DataType {
    var enumSpec: Option[EnumSpec] = None
  }

  case class SwitchType(on: Ast.expr, cases: Map[Ast.expr, DataType]) extends DataType

  object SwitchType {
    /**
      * Constant that would be used for "else" case in SwitchType case class "cases" map.
      */
    val ELSE_CONST = Ast.expr.Name(Ast.identifier("_"))
  }

  private val ReIntType = """([us])(2|4|8)(le|be)?""".r
  private val ReFloatType = """f(4|8)(le|be)?""".r
  private val ReBitType= """b(\d+)""".r

  def fromYaml(
    dto: Option[String],
    path: List[String],
    metaDef: MetaDefaults,
    arg: YamlAttrArgs
  ): DataType = {
    val r = dto match {
      case None =>
        arg.contents match {
          case Some(c) => FixedBytesType(c, arg.process)
          case _ => arg.getByteArrayType(path)
        }
      case Some(dt) => dt match {
        case "u1" => Int1Type(false)
        case "s1" => Int1Type(true)
        case ReIntType(signStr, widthStr, endianStr) =>
          IntMultiType(
            signStr match {
              case "s" => true
              case "u" => false
            },
            widthStr match {
              case "2" => Width2
              case "4" => Width4
              case "8" => Width8
            },
            Endianness.fromString(Option(endianStr), metaDef.endian, dt, path)
          )
        case ReFloatType(widthStr, endianStr) =>
          FloatMultiType(
            widthStr match {
              case "4" => Width4
              case "8" => Width8
            },
            Endianness.fromString(Option(endianStr), metaDef.endian, dt, path)
          )
        case ReBitType(widthStr) =>
          (arg.enumRef, widthStr.toInt) match {
            case (None, 1) =>
              // if we're not inside enum and it's 1-bit type
              BitsType1
            case (_, width) =>
              // either inside enum (any width) or (width != 1)
              BitsType(width)
          }
        case "str" | "strz" =>
          val enc = getEncoding(arg.encoding, metaDef, path)

          // "strz" makes terminator = 0 by default
          val arg2 = if (dt == "strz") {
            arg.copy(terminator = arg.terminator.orElse(Some(0)))
          } else {
            arg
          }

          val bat = arg2.getByteArrayType(path)
          StrFromBytesType(bat, enc)
        case _ =>
          val dtl = classNameToList(dt)
          if (arg.size.isEmpty && !arg.sizeEos && arg.terminator.isEmpty) {
            if (arg.process.isDefined)
              throw new YAMLParseException(s"user type '$dt': need 'size' / 'size-eos' / 'terminator' if 'process' is used", path)
            UserTypeInstream(dtl, arg.parent)
          } else {
            val bat = arg.getByteArrayType(path)
            UserTypeFromBytes(dtl, arg.parent, bat, arg.process)
          }
      }
    }

    arg.enumRef match {
      case Some(enumName) =>
        r match {
          case numType: IntType => EnumType(classNameToList(enumName), numType)
          case _ =>
            throw new YAMLParseException(s"tried to resolve non-integer $r to enum", path)
        }
      case None =>
        r
    }
  }

  def getEncoding(curEncoding: Option[String], metaDef: MetaDefaults, path: List[String]): String = {
    curEncoding.orElse(metaDef.encoding) match {
      case Some(enc) => enc
      case None =>
        throw new YAMLParseException("string type, but no encoding found", path)
    }
  }

  /**
    * Splits complex class name notation (which may name class hierarchy `something::like::that`)
    * into components, represented by a list of strings.
    * @param s class name notation as string
    * @return class name notation as list of components
    */
  def classNameToList(s: String): List[String] = s.split("::", -1).toList
}