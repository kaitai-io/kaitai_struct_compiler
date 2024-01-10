package io.kaitai.struct.datatype

import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.format._
import io.kaitai.struct.problems.KSYParseError
import io.kaitai.struct.translators.TypeDetector

sealed trait DataType {
  /**
    * @return Data type as non-owning data type. Default implementation
    *         always returns itself, complex types
    */
  def asNonOwning(isOwningInExpr: Boolean = false): DataType = this
}

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
  sealed trait ReadableType extends DataType {
    def apiCall(defEndian: Option[FixedEndian]): String
  }

  abstract sealed class NumericType extends DataType
  abstract sealed class BooleanType extends DataType

  abstract sealed class IntType extends NumericType
  case object CalcIntType extends IntType
  case class Int1Type(signed: Boolean) extends IntType with ReadableType {
    override def apiCall(defEndian: Option[FixedEndian]): String = if (signed) "s1" else "u1"
  }
  case class IntMultiType(signed: Boolean, width: IntWidth, endian: Option[FixedEndian]) extends IntType with ReadableType {
    override def apiCall(defEndian: Option[FixedEndian]): String = {
      val ch1 = if (signed) 's' else 'u'
      val finalEnd = endian.orElse(defEndian)
      s"$ch1${width.width}${finalEnd.map(_.toSuffix).getOrElse("")}"
    }
  }
  case class BitsType1(bitEndian: BitEndianness) extends BooleanType
  case class BitsType(width: Int, bitEndian: BitEndianness) extends IntType

  abstract class FloatType extends NumericType
  case object CalcFloatType extends FloatType
  case class FloatMultiType(width: IntWidth, endian: Option[FixedEndian]) extends FloatType with ReadableType {
    override def apiCall(defEndian: Option[FixedEndian]): String = {
      val finalEnd = endian.orElse(defEndian)
      s"f${width.width}${finalEnd.map(_.toSuffix).getOrElse("")}"
    }
  }

  trait Processing {
    def process: Option[ProcessExpr]
  }

  abstract class BytesType extends DataType with Processing
  case object CalcBytesType extends BytesType {
    override def process = None
  }
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
  case class StrFromBytesType(bytes: BytesType, var encoding: String) extends StrType

  case object CalcBooleanType extends BooleanType

  /**
    * Complex data type is a data type which creation and destruction is
    * not an atomic, built-in operation, but rather a sequence of new/delete
    * operations. The main common trait for all complex data types is a flag
    * that determines whether they're "owning" or "borrowed". Owning objects
    * manage their own creation/destruction, borrowed rely on other doing
    * that.
    */
  abstract sealed class ComplexDataType extends DataType {
    /**
      * @return If true, this is "owning" type: for languages where data ownership
      *         matters, this one represents primary owner of the data block, who
      *         will be responsible for whole life cycle: creation of the object
      *         and its destruction.
      */
    def isOwning: Boolean
    /**
      * @return If true, this type acts like an owning type in expressions, BUT the
      *         data block is already owned by another type.
      *
      *         The only situation when this can be `true` is when subscripting
      *         an array with owning inner type. Although the element
      *         obtained from such an array has the "owning" type (behaves in
      *         expressions like that), we can't save it in a value instance with
      *         owning type as well, because the data block representing the element
      *         is already owned by the array, so the instance CAN'T OWN IT too.
      *         So [[isOwning]] = false and [[isOwningInExpr]] = true.
      *
      *         If [[isOwning]] == true, [[isOwningInExpr]] can't be `true` as well,
      *         this MUST stay `false` in owning types.
      */
    def isOwningInExpr: Boolean = false
  }

  abstract class StructType extends ComplexDataType

  /**
    * Common abstract ancestor for all types which can treated as "user types".
    * Namely, this typically means that this type has a name, may have some
    * parameters, and forced parent expression.
    * @param name name of the type, might include several components
    * @param forcedParent optional parent enforcement expression
    * @param args parameters passed into this type as extra arguments
    */
  abstract class UserType(
    val name: List[String],
    val forcedParent: Option[Ast.expr],
    var args: Seq[Ast.expr]
  ) extends StructType {
    var classSpec: Option[ClassSpec] = None
    def isOpaque = {
      val cs = classSpec.get
      cs.isTopLevel || cs.meta.isOpaque
    }

  }
  case class UserTypeInstream(
    _name: List[String],
    _forcedParent: Option[Ast.expr],
    _args: Seq[Ast.expr] = Seq()
  ) extends UserType(_name, _forcedParent, _args) {
    def isOwning = true
    override def asNonOwning(isOwningInExpr: Boolean = false): UserType = {
      val r = CalcUserType(name, forcedParent, args, isOwningInExpr)
      r.classSpec = classSpec
      r
    }
  }
  case class UserTypeFromBytes(
    _name: List[String],
    _forcedParent: Option[Ast.expr],
    _args: Seq[Ast.expr] = Seq(),
    bytes: BytesType,
    override val process: Option[ProcessExpr]
  ) extends UserType(_name, _forcedParent, _args) with Processing {
    override def isOwning = true
    override def asNonOwning(isOwningInExpr: Boolean = false): UserType = {
      val r = CalcUserTypeFromBytes(name, forcedParent, args, bytes, process, isOwningInExpr)
      r.classSpec = classSpec
      r
    }
  }
  case class CalcUserType(
    _name: List[String],
    _forcedParent: Option[Ast.expr],
    _args: Seq[Ast.expr] = Seq(),
    override val isOwningInExpr: Boolean = false
  ) extends UserType(_name, _forcedParent, _args) {
    override def isOwning = false
  }
  case class CalcUserTypeFromBytes(
    _name: List[String],
    _forcedParent: Option[Ast.expr],
    _args: Seq[Ast.expr] = Seq(),
    bytes: BytesType,
    override val process: Option[ProcessExpr],
    override val isOwningInExpr: Boolean = false
  ) extends UserType(_name, _forcedParent, _args) with Processing {
    override def isOwning = false
  }

  abstract sealed class ArrayType(val elType: DataType) extends ComplexDataType

  case class ArrayTypeInStream(_elType: DataType) extends ArrayType(_elType) {
    override def isOwning: Boolean = true
    override def asNonOwning(isOwningInExpr: Boolean = false): CalcArrayType = CalcArrayType(elType, isOwningInExpr)
  }
  case class CalcArrayType(_elType: DataType, override val isOwningInExpr: Boolean = false) extends ArrayType(_elType) {
    override def isOwning: Boolean = false
  }

  val USER_TYPE_NO_PARENT = Ast.expr.Bool(false)

  // 2024/1/10 change: why add calc any type here
  case object AnyType extends DataType
  case object CalcAnyType extends DataType

  case object KaitaiStructType extends StructType {
    def isOwning = true
    override def asNonOwning(isOwningInExpr: Boolean = false): DataType = CalcKaitaiStructType(isOwningInExpr)
  }
  case class CalcKaitaiStructType(override val isOwningInExpr: Boolean = false) extends StructType {
    def isOwning = false
  }
  case object OwnedKaitaiStreamType extends ComplexDataType {
    def isOwning = true
    override def asNonOwning(isOwningInExpr: Boolean = false): DataType = KaitaiStreamType
  }
  case object KaitaiStreamType extends ComplexDataType {
    def isOwning = false
  }

  case class EnumType(name: List[String], basedOn: IntType) extends DataType {
    var enumSpec: Option[EnumSpec] = None
  }

  case class SwitchType(
    on: Ast.expr,
    cases: Map[Ast.expr, DataType],
    isOwning: Boolean = true,
    override val isOwningInExpr: Boolean = false
  ) extends ComplexDataType {
    def combinedType: DataType = TypeDetector.combineTypes(cases.values)

    /**
      * @return True if this switch type includes an "else" case
      */
    def hasElseCase: Boolean = cases.contains(SwitchType.ELSE_CONST)

    /**
      * If a switch type has no else statement, it will turn out to be null
      * every case would fail, so it's nullable.
      * @return True if this switch type is nullable for regular languages.
      */
    def isNullable: Boolean = !hasElseCase

    /**
      * @return True if this switch type is nullable in a raw switch bytes languages (C++).
      */
    def isNullableSwitchRaw: Boolean = {
      val elseCase = cases.get(SwitchType.ELSE_CONST)
      elseCase match {
        case Some(_: BytesType) =>
          // else case with bytes type, nullable for C++-like languages
          true
        case Some(x) =>
          // else case with any user type, non-nullable
          false
        case None =>
          // no else case, even raw bytes, definitely nullable
          true
      }
    }

    def hasSize: Boolean =
      cases.values.exists((t) =>
        t.isInstanceOf[UserTypeFromBytes] || t.isInstanceOf[BytesType]
      )

    override def asNonOwning(isOwningInExpr: Boolean = false): DataType = SwitchType(on, cases, false, isOwningInExpr)
  }

  object SwitchType {
    /**
      * Constant that would be used for "else" case in SwitchType case class "cases" map.
      */
    val ELSE_CONST = Ast.expr.Name(Ast.identifier("_"))

    val LEGAL_KEYS_SWITCH = Set(
      "switch-on",
      "cases"
    )

    def fromYaml1(switchSpec: Map[String, Any], path: List[String]): (String, Map[String, String]) = {
      val _on = ParseUtils.getValueStr(switchSpec, "switch-on", path)
      val _cases: Map[String, String] = switchSpec.get("cases") match {
        case None => Map()
        case Some(x) => ParseUtils.asMapStrStr(x, path ++ List("cases"))
      }

      ParseUtils.ensureLegalKeys(switchSpec, LEGAL_KEYS_SWITCH, path)
      (_on, _cases)
    }

    def fromYaml(
      switchSpec: Map[String, Any],
      path: List[String],
      metaDef: MetaSpec,
      arg: YamlAttrArgs
    ): SwitchType = {
      val (_on, _cases) = fromYaml1(switchSpec, path)

      val on = Expressions.parse(_on)
      val cases: Map[Ast.expr, DataType] = _cases.map { case (condition, typeName) =>
        Expressions.parse(condition) -> DataType.fromYaml(
          Some(typeName), path ++ List("cases"), metaDef,
          arg
        )
      }

      // If we have size defined, and we don't have any "else" case already, add
      // an implicit "else" case that will at least catch everything else as
      // "untyped" byte array of given size
      val addCases: Map[Ast.expr, DataType] = if (cases.contains(ELSE_CONST)) {
        Map()
      } else {
        (arg.size, arg.sizeEos) match {
          case (Some(sizeValue), false) =>
            Map(SwitchType.ELSE_CONST -> BytesLimitType(sizeValue, None, false, None, arg.process))
          case (None, true) =>
            Map(SwitchType.ELSE_CONST -> BytesEosType(None, false, None, arg.process))
          case (None, false) =>
            Map()
          case (Some(_), true) =>
            throw KSYParseError("can't have both `size` and `size-eos` defined", path).toException
        }
      }

      SwitchType(on, cases ++ addCases)
    }
  }

  private val ReIntType = """([us])(2|4|8)(le|be)?""".r
  private val ReFloatType = """f(4|8)(le|be)?""".r
  private val ReBitType = """b(\d+)(le|be)?""".r

  def fromYaml(
    dto: Option[String],
    path: List[String],
    metaDef: MetaSpec,
    arg: YamlAttrArgs
  ): DataType = {
    val r = dto match {
      case None =>
        arg.contents match {
          case Some(c) => BytesLimitType(Ast.expr.IntNum(c.length), None, false, None, arg.process)
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
        case ReBitType(widthStr, bitEndianStr) =>
          (arg.enumRef, widthStr.toInt) match {
            case (None, 1) =>
              // if we're not inside enum and it's 1-bit type
              BitsType1(
                BitEndianness.fromString(Option(bitEndianStr), metaDef.bitEndian, dt, path)
              )
            case (_, width) =>
              // either inside enum (any width) or (width != 1)
              BitsType(
                width,
                BitEndianness.fromString(Option(bitEndianStr), metaDef.bitEndian, dt, path)
              )
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
          val typeWithArgs = Expressions.parseTypeRef(dt)
          if (arg.size.isEmpty && !arg.sizeEos && arg.terminator.isEmpty) {
            if (arg.process.isDefined)
              throw KSYParseError(s"user type '$dt': need 'size' / 'size-eos' / 'terminator' if 'process' is used", path).toException
            UserTypeInstream(
              typeWithArgs.typeName.names.toList,
              arg.parent,
              typeWithArgs.arguments.elts
            )
          } else {
            val bat = arg.getByteArrayType(path)
            UserTypeFromBytes(
              typeWithArgs.typeName.names.toList,
              arg.parent,
              typeWithArgs.arguments.elts,
              bat,
              arg.process
            )
          }
      }
    }

    applyEnumType(r, arg.enumRef, path)
  }

  private def applyEnumType(r: DataType, enumRef: Option[String], path: List[String]) = {
    enumRef match {
      case Some(enumName) =>
        r match {
          case numType: IntType => EnumType(classNameToList(enumName), numType)
          case _ =>
            throw KSYParseError(s"tried to resolve non-integer $r to enum", path).toException
        }
      case None =>
        r
    }
  }

  private val RePureIntType = """([us])(2|4|8)""".r
  private val RePureFloatType = """f(4|8)""".r

  def pureFromString(dto: Option[String], enumRef: Option[String], path: List[String]): DataType =
    applyEnumType(pureFromString(dto), enumRef, path)

  def pureFromString(dto: Option[String]): DataType = dto match {
    case None => CalcBytesType
    case Some(dt) => pureFromString(dt)
  }

  def pureFromString(dt: String): DataType = {
    val arraySuffix = "[]"
    val (isArray, singleId) = (dt.endsWith(arraySuffix), dt.stripSuffix(arraySuffix))
    val singleType = singleId match {
      case "bytes" => CalcBytesType
      case "u1" => Int1Type(false)
      case "s1" => Int1Type(true)
      case RePureIntType(signStr, widthStr) =>
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
          None
        )
      case RePureFloatType(widthStr) =>
        FloatMultiType(
          widthStr match {
            case "4" => Width4
            case "8" => Width8
          },
          None
        )
      case ReBitType(widthStr, bitEndianStr) =>
        widthStr match {
          // bit endianness is not applicable here (and the dependent code doesn't care about it), so why not assume big endian
          case "1" => BitsType1(BigBitEndian)
          case _ => BitsType(widthStr.toInt, BigBitEndian)
        }
      case "str" => CalcStrType
      case "bool" => CalcBooleanType
      case "struct" => CalcKaitaiStructType()
      case "io" => KaitaiStreamType
      case "any" => AnyType
      case _ => CalcUserType(classNameToList(singleId), None)
    }
    if (isArray) {
      CalcArrayType(singleType)
    } else {
      singleType
    }
  }

  def getEncoding(curEncoding: Option[String], metaDef: MetaSpec, path: List[String]): String = {
    curEncoding.orElse(metaDef.encoding) match {
      case Some(enc) => enc
      case None =>
        throw KSYParseError("string type, but no encoding found", path).toException
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
