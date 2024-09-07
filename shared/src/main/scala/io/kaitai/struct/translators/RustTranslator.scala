package io.kaitai.struct.translators

import io.kaitai.struct.format._
import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.{EnumSpec, Identifier, IoIdentifier, ParentIdentifier, RootIdentifier}
import io.kaitai.struct.languages.RustCompiler
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class RustTranslator(provider: TypeProvider, config: RuntimeConfig)
  extends BaseTranslator(provider) {

  import RustCompiler._

  var lastFoundMemberClass: ClassSpec = provider.nowClass

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "vec![" + arr.map(x => "%0#2xu8".format(x & 0xff)).mkString(", ") + "]"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    "vec![" + elts.map(translate).mkString(", ") + "]"
  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String = {
    t match {
      case CalcStrType => "vec![" + value.map(v => translate(v)).mkString(".to_string(), ") + ".to_string()]"
      case _ => "vec![" + value.map(v => translate(v)).mkString(", ") + "]"
    }
  }

  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\"
  )

  override def strLiteralGenericCC(code: Char): String =
    strLiteralUnicode(code)

  override def strLiteralUnicode(code: Char): String =
    "\\u{%x}".format(code.toInt)

  def isSignedIntType(dt: DataType): Boolean = dt match {
    case Int1Type(true) => true
    case IntMultiType(true, _, _) => true
    case CalcIntType => true
    case _ => false
  }

  def isAllDigits(x: String) = x forall Character.isDigit

  override def genericBinOp(left: Ast.expr,
                            op: Ast.operator,
                            right: Ast.expr,
                            extPrec: Int): String = {
    val lt = detectType(left)
    val rt = detectType(right)
    val tl = translate(left)
    val tr = translate(right)

    if (isSignedIntType(lt) && isSignedIntType(rt) && op == Ast.operator.Mod) {
        s"modulo($tl as i64, $tr as i64)"
    } else if (isSignedIntType(lt) && isSignedIntType(rt) && op == Ast.operator.RShift) {
        // Arithmetic right shift on signed integer types, logical right shift on unsigned integer types
        val ct = RustCompiler.kaitaiPrimitiveToNativeType(TypeDetector.combineTypes(lt, rt))
        s"((($tl as u64) >> $tr) as $ct)"
    } else {
      if (lt == rt && isAllDigits(tl) && isAllDigits(tr)) {
        // let rust decide final type
        s"($tl ${binOp(op)} $tr)"
      } else {
        val ct = RustCompiler.kaitaiPrimitiveToNativeType(TypeDetector.combineTypes(lt, rt))
        s"(($tl as $ct) ${binOp(op)} ($tr as $ct))"
      }
    }
  }

  def unwrap(s: String): String = s + ".as_ref().unwrap()"

  override def doName(s: String): String = s match {
    case Identifier.PARENT => s
    case _ =>
      val refOpt = "^Option<.*".r
      val memberFound = findMember(s)
      val f = s"$s()"
      if (memberFound.isDefined) {
        memberFound.get match {
          case vis: ValueInstanceSpec =>
            val aType = RustCompiler.kaitaiTypeToNativeType(Some(vis.id), provider.nowClass, vis.dataTypeComposite)
            aType match {
              case refOpt() => unwrap(s"$f?")
              case _ => s"$f?"
            }
          case as: AttrSpec =>
            val aType = RustCompiler.kaitaiTypeToNativeType(Some(as.id), provider.nowClass, as.dataTypeComposite)
            aType match {
              case refOpt() =>
                if (!enum_numeric_only(as.dataTypeComposite)) {
                  unwrap(f)
                } else f
              case _ => f
            }
          case pd: ParamDefSpec =>
            val aType = RustCompiler.kaitaiTypeToNativeType(Some(pd.id), provider.nowClass, pd.dataTypeComposite)
            aType match {
              case refOpt() => unwrap(f)
              case _ => f
            }
          case pis: ParseInstanceSpec =>
            val aType = RustCompiler.kaitaiTypeToNativeType(Some(pis.id), provider.nowClass, pis.dataTypeComposite)
            aType match {
              case refOpt() => unwrap(s"$f?")
              case _ => s"$f?"
            }
          case _ =>
            f
        }
      }
      else {
        f
      }
    }

  def updateLastFoundMemberClass(dt: DataType) {
    if (dt.isInstanceOf[UserType]) {
      val s = dt.asInstanceOf[UserType]
      if (s.classSpec.isDefined) {
        lastFoundMemberClass = s.classSpec.get
      }
    }
  }

  def resetLastFoundMemberClass() {
    lastFoundMemberClass = provider.nowClass
  }

  def findMember(attrName: String, c: ClassSpec = lastFoundMemberClass): Option[MemberSpec] = {
    def findInClass(inClass: ClassSpec): Option[MemberSpec] = {

      inClass.seq.foreach { el =>
        if (idToStr(el.id) == attrName) {
          updateLastFoundMemberClass(el.dataType)
          return Some(el)
        }
      }

      inClass.params.foreach { el =>
        if (idToStr(el.id) == attrName) {
          updateLastFoundMemberClass(el.dataType)
          return Some(el)
        }
      }

      inClass.instances.foreach { case (instName, instSpec) =>
        if (idToStr(instName) == attrName) {
          updateLastFoundMemberClass(instSpec.dataType)
          return Some(instSpec)
        }
      }

      inClass.types.foreach{ t =>
        for { found <- findInClass(t._2) }
          return Some(found)
      }
      None
    }

    attrName match {
      case Identifier.PARENT | Identifier.IO =>
        return None
      case _ =>
        for { ms <- findInClass(c) }
          return Some(ms)

        provider.asInstanceOf[ClassTypeProvider].allClasses.foreach { cls =>
          for { ms <- findInClass(cls._2) }
            return Some(ms)
        }
    }
    None
  }

  def get_instance(cs: ClassSpec, s: String): Option[InstanceSpec] = {
    var found : Option[InstanceSpec] = None
    // look for instance
    cs.instances.foreach { case (instName, instSpec) =>
      if (idToStr(instName) == s) {
        found = Some(instSpec)
      }
    }
    // look deeper
    if (found.isEmpty) {
      cs.types.foreach {
        case (_, typeSpec) =>
          found = get_instance(typeSpec, s)
          if (found.isDefined) {
            return found
          }
        }
    }
    found
  }

  override def anyField(value: expr, attrName: String): String = {
    resetLastFoundMemberClass()
    val t = translate(value)
    var a = doName(attrName)
    attrName match {
      case Identifier.PARENT => a = a + unwrap(".get_value().borrow().upgrade()")
      case _ =>
    }
    var r = ""
    if (need_deref(attrName)) {
      if (t.charAt(0) == '*') {
        r = s"$t.$a"
      } else {
        r = s"*$t.$a"
      }
    } else {
      if (t.charAt(0) == '*') {
        r = s"${t.substring(1)}.$a"
      } else {
        r = s"$t.$a"
      }
    }
    r
  }

  def rem_vec_amp(s: String): String = {
    if (s.startsWith("&vec!")) {
      s.substring(1)
    } else {
      s
    }
  }

  def ensure_vec_amp(s: String): String = {
    if (s.startsWith("vec!")) {
      s"&$s"
    } else {
      s
    }
  }

  def ensure_amp(s: String): String = {
    if (s.charAt(0) == '&') {
      s
    } else {
      s"&$s"
    }
  }

  def remove_deref(s: String): String = {
    if (s.charAt(0) == '*') {
      s.substring(1)
    } else {
      s
    }
  }

  def ensure_deref(s: String): String = {
    if (s.startsWith(self_name())) {
      s"*$s"
    } else {
      s
    }
  }

  def enum_numeric_only(dataType: DataType): Boolean = {
    var types : Set[DataType] = Set()
    var enum_typename = false
    dataType match {
      case st: SwitchType =>
        types = st.cases.values.toSet
        enum_typename = true
      //case _: EnumType => return true
      case _ => return false
    }
    var enum_only_numeric = true
    types.foreach {
      case _: NumericType => // leave unchanged
      case _ => enum_only_numeric = false
    }
    enum_only_numeric
  }

  def is_copy_type(dataType: DataType): Boolean = dataType match {
    case _: SwitchType => false
    case _: UserType => false
    case _: BytesType => false
    case _: ArrayType => false
    case _: StrType => false
    case _: EnumType => false
    case _ => true
  }

  def need_deref(s: String, c: ClassSpec = provider.nowClass): Boolean = {
    var deref = false
    val memberFound = findMember(s, c)
    if (memberFound.isDefined ) {
      val spec = memberFound.get
      spec match {
        case _: AttrSpec | _: ParamDefSpec =>
          deref = !enum_numeric_only(spec.dataTypeComposite)
        case _: ValueInstanceSpec | _: ParseInstanceSpec =>
          deref = true
        case _ =>
      }
    }
    deref
  }

  override def doLocalName(s: String): String = s match {
    case Identifier.ITERATOR => "_tmpa"
    case Identifier.ITERATOR2 => "_tmpb"
    case Identifier.INDEX => "_i"
    case Identifier.IO => s"${RustCompiler.privateMemberName(IoIdentifier)}"
    case Identifier.ROOT => "_r"
    case Identifier.PARENT => unwrap("_prc")
    case _ =>
      // reset "looking for variable" context
      resetLastFoundMemberClass()
      val n = doName(s)
      val deref = !n.endsWith(".as_str()") && !n.endsWith(".as_slice()") && need_deref(s)
      if (deref) {
        s"*${self_name()}.$n"
      } else {
        s"${self_name()}.$n"
      }
  }
  override def doEnumCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"

  override def doInternalName(id: Identifier): String =
    s"${doLocalName(idToStr(id))}"

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String =
    s"${RustCompiler.types2class(enumSpec.name)}::${Utils.upperCamelCase(label)}"

  override def doNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    val lt = detectType(left)
    val rt = detectType(right)
    if (lt != rt) {
      val ct = RustCompiler.kaitaiPrimitiveToNativeType(TypeDetector.combineTypes(lt, rt))
      s"((${translate(left)} as $ct) ${cmpOp(op)} (${translate(right)} as $ct))"
    } else {
      s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
    }
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"${ensure_deref(translate(left))} ${cmpOp(op)} ${remove_deref(translate(right))}.to_string()"

  override def doEnumById(enumSpec: EnumSpec, id: String): String =
    s"($id as i64).try_into()?"

  override def arraySubscript(container: expr, idx: expr): String =
    s"${remove_deref(translate(container))}[${translate(idx)} as usize]"

  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String = {
    var to_type = ""
    detectType(ifTrue) match {
      case _: UserType => to_type = ".clone()"
      case _: EnumType => to_type = ".clone()"
      case _: StrType => to_type = ".to_string()"
      case _: BytesType => to_type = ".to_vec()"
      case _: CalcArrayType => to_type = ".clone()"
      case _ =>
    }
    if (to_type.isEmpty) {
      s"if ${translate(condition)} { ${translate(ifTrue)} } else { ${translate(ifFalse)} }"
    } else {
      s"if ${translate(condition)} { ${remove_deref(translate(ifTrue))}$to_type } else { ${remove_deref(translate(ifFalse))}$to_type }"
    }
  }

  override def doCast(value: Ast.expr, castTypeName: DataType): String = {
    val value_type = detectType(value)
    if(castTypeName == value_type)
      return translate(value)

    val ct = RustCompiler.kaitaiTypeToNativeType(None, provider.nowClass, castTypeName, excludeOptionWrapper = true)
    var into = false
    castTypeName match {
      case _: UserType => into = true;
      case CalcBytesType => into = true;
      case _ =>
    }
    if (into) {
      s"Into::<$ct>::into(&${translate(value)})"
    } else {
      s"(${translate(value)} as $ct)"
    }
  }

  override def translate(v: Ast.expr): String = {
    v match {
      case Ast.expr.EnumById(enumType, id, inType) =>
        id match {
          case ifExp: Ast.expr.IfExp =>
            val enumSpec = provider.resolveEnum(inType, enumType.name)
            val enumName = RustCompiler.types2class(enumSpec.name)
            def toStr(ex: Ast.expr) = ex match {
              case Ast.expr.IntNum(n) => s"$enumName::try_from($n)?"
              case _ => super.translate(ex)
            }
            val ifTrue = toStr(ifExp.ifTrue)
            val ifFalse = toStr(ifExp.ifFalse)

            "if " + translate(ifExp.condition) + s" { $ifTrue } else { $ifFalse }"
          case _ => super.translate(v)
        }
      case _ =>
        super.translate(v)
    }
  }

  override def strConcat(left: Ast.expr, right: Ast.expr, extPrec: Int): String =
    s"""format!("{}{}", ${translate(left)}, ${translate(right)})"""

  override def doInterpolatedStringLiteral(exprs: Seq[Ast.expr]): String =
    if (exprs.isEmpty) {
      doStringLiteral("")
    } else { // format!("{expr1}{expr2}{expr3}")
      var s = "format!(\""
      exprs.foreach(i => { s+= "{}" })
      s += "\", "
      s += exprs.map(translate).mkString(", ")
      s += ")"
      s
    }

  override def strToInt(s: expr, base: expr): String =
    translate(base) match {
      case "10" =>
        s"${translate(s)}.parse::<i32>().map_err(|_| KError::CastError)?"
      case _ =>
        s"i32::from_str_radix(${translate(s)}, ${translate(base)}).map_err(|_| KError::CastError)?"
    }

  override def enumToInt(v: expr, et: EnumType): String =
    s"i64::from(&${translate(v)})"

  override def boolToInt(v: expr): String =
    s"(${translate(v)}) as i32"

  override def floatToInt(v: expr): String =
    s"${translate(v)} as i32"

  override def intToStr(i: expr): String =
    s"${remove_deref(translate(i))}.to_string()"

  override def bytesToStr(bytesExpr: String, encoding: String): String =
    s"""bytes_to_str(&$bytesExpr, "$encoding")?"""

  override def bytesLength(b: Ast.expr): String =
    s"${remove_deref(translate(b))}.len()"

  override def strLength(s: expr): String =
    s"${remove_deref(translate(s))}.len()"

  override def strReverse(s: expr): String = {
    val e = translate(s)
    if (e.charAt(0) == '*')
      s"reverse_string(&$e)?"
    else
      s"reverse_string($e)?"
  }
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}[${translate(from)}..${translate(to)}]"

  override def arrayFirst(a: expr): String =
    s"${ensure_deref(translate(a))}.first().ok_or(KError::EmptyIterator)?"
  override def arrayLast(a: expr): String =
    s"${ensure_deref(translate(a))}.last().ok_or(KError::EmptyIterator)?"
  override def arraySize(a: expr): String =
    s"${remove_deref(translate(a))}.len()"

  def is_float_type(a: Ast.expr): Boolean = {
    detectType(a) match {
      case t: CalcArrayType =>
        t.elType match {
          case _: FloatMultiType => true
          case CalcFloatType => true
          case _ => false
        }
      case t: ArrayType =>
        t.elType match  {
          case _: FloatMultiType => true
          case _ => false
        }
      case _ => false
    }
  }

  override def arrayMin(a: Ast.expr): String = {
    if (is_float_type(a)) {
      s"${ensure_deref(translate(a))}.iter().reduce(|a, b| if (a.min(*b)) == *b {b} else {a}).ok_or(KError::EmptyIterator)?"
    } else {
      s"${ensure_deref(translate(a))}.iter().min().ok_or(KError::EmptyIterator)?"
    }
  }

  override def arrayMax(a: Ast.expr): String = {
    if (is_float_type(a)) {
      s"${ensure_deref(translate(a))}.iter().reduce(|a, b| if (a.max(*b)) == *b {b} else {a}).ok_or(KError::EmptyIterator)?"
    } else {
      s"${ensure_deref(translate(a))}.iter().max().ok_or(KError::EmptyIterator)?"
    }
  }
}
