package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, RuntimeConfig, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format.{NoRepeat, RepeatEos, RepeatExpr, RepeatSpec, _}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{BaseTranslator, JavaTranslator, TypeProvider}

class PHPCompiler(verbose: Boolean, out: LanguageOutputWriter, namespace: String = "")
  extends LanguageCompiler(verbose, out)
    with AllocateIOLocalVar
    with UniversalFooter
    with EveryReadIsExpression {

  import PHPCompiler._

  override def getStatic = PHPCompiler

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def fileHeader(topClassName: String): Unit = {
    out.puts("<?php")
    out.puts(s"// $headerComment")
    if (!namespace.isEmpty) {
      out.puts
      out.puts(s"namespace $namespace;")
    }

    out.puts
  }

  override def classHeader(name: List[String]): Unit = {
    out.puts(s"class ${types2class(name)} extends $kstructName {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = universalFooter

  override def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit = {
    out.puts
    out.puts(
      "public function __construct(" +
      kstreamName + " $io, " +
      types2class(parentClassName) + " $parent = null, " +
      types2class(rootClassName) + " $root = null) {"
    )
    out.inc
    out.puts("parent::__construct($io, $parent, $root);")
    out.puts("$this->_parse();")
    out.dec
    out.puts("}")

    out.puts("private function _parse() {")
    out.inc
  }

  override def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    attrName match {
      case ParentIdentifier | RootIdentifier =>
        // just ignore it for now
      case _ =>
        out.puts(s"protected $$${idToStr(attrName)};")
    }
  }

  override def attributeReader(attrName: Identifier, attrType: BaseType): Unit = {
    attrName match {
      case ParentIdentifier | RootIdentifier =>
        // just ignore it for now
      case _ =>
        out.puts(s"public function ${publicMemberName(attrName)}() { return ${privateMemberName(attrName)}; }")
    }
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = {
    out.puts(s"${privateMemberName(attrName)} = $normalIO->ensureFixedContents(${contents.length}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"$destName = $kstreamName->processXor($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$destName = $kstreamName->processZlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName->processRotateLeft($srcName, $expr, 1);")
    }
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val ioName = s"_io_${idToStr(id)}"

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$memberName.get($memberName.size() - 1)"
      case NoRepeat => memberName
    }

    out.puts(s"$kstreamName $ioName = new $kstreamName($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"$kstreamName io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"long _pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io->seek(_pos);")

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2JavaType(ArrayType(dataType))}();")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2JavaType(ArrayType(dataType))}((int) (${expression(repeatExpr)}));")
    out.puts(s"for ($$i = 0; $$i < ${expression(repeatExpr)}; $$i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read${Utils.capitalize(t.apiCall)}()"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io->readStrByteLimit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + "->readStrEos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + "->readStrz(\"" + encoding + '"' + s", $terminator, $include, $consume, $eosError)"
      case EnumType(enumName, t) =>
        s"${type2class(enumName)}.byId(${parseExpr(t, io)})"
      case BytesLimitType(size, _) =>
        s"$io->readBytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io->readBytesFull()"
      case t: UserType =>
        s"new ${types2class(t.name)}($io, this, _root)"
    }
  }

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"private ${kaitaiType2JavaTypeBoxed(attrType)} ${idToStr(attrName)};")
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: BaseType): Unit = {
    out.puts(s"public ${kaitaiType2JavaTypeBoxed(dataType)} ${idToStr(instName)}() throws IOException {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (${privateMemberName(instName)} != null)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Map[Long, String]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"public enum $enumClass {")
    out.inc

    val it = enumColl.toIterable
    if (enumColl.size > 1) {
      it.dropRight(1).foreach { case (id, label) =>
        out.puts(s"${value2Const(label)}($id),")
      }
    }
    it.last match {
      case (id, label) =>
        out.puts(s"${value2Const(label)}($id);")
    }

    out.puts
    out.puts("private final long id;")
    out.puts(s"$enumClass(long id) { this.id = id; }")
    out.puts("public long id() { return id; }")
    out.puts(s"private static final Map<Long, $enumClass> byId = new HashMap<Long, $enumClass>(${enumColl.size});")
    out.puts("static {")
    out.inc
    out.puts(s"for ($enumClass e : $enumClass.values())")
    out.inc
    out.puts(s"byId.put(e.id(), e);")
    out.dec
    out.dec
    out.puts("}")
    out.puts(s"public static $enumClass byId(long id) { return byId.get(id); }")
    out.dec
    out.puts("}")
  }

  def value2Const(s: String) = s.toUpperCase

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = s"$$this->${idToStr(id)}"

  override def publicMemberName(id: Identifier) = idToStr(id)
}

object PHPCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaTranslator(tp)
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.php"

  override def kstreamName: String = "\\Kaitai\\Struct\\Stream"

  override def kstructName: String = "\\Kaitai\\Struct\\Struct"

  def kaitaiType2JavaType(attrType: BaseType): String = kaitaiType2JavaTypePrim(attrType)

  /**
    * Determine Java data type corresponding to a KS data type. A "primitive" type (i.e. "int", "long", etc) will
    * be returned if possible.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypePrim(attrType: BaseType): String = {
    attrType match {
      case Int1Type(false) => "int"
      case IntMultiType(false, Width2, _) => "int"
      case IntMultiType(false, Width4, _) => "long"
      case IntMultiType(false, Width8, _) => "long"

      case Int1Type(true) => "byte"
      case IntMultiType(true, Width2, _) => "short"
      case IntMultiType(true, Width4, _) => "int"
      case IntMultiType(true, Width8, _) => "long"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case CalcIntType => "int"
      case CalcFloatType => "double"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => type2class(name)

      case ArrayType(inType) => kaitaiType2JavaTypeBoxed(attrType)
    }
  }

  /**
    * Determine Java data type corresponding to a KS data type. A non-primitive type (i.e. "Integer", "Long", etc) will
    * be returned, to be used when proper objects should be used.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypeBoxed(attrType: BaseType): String = {
    attrType match {
      case Int1Type(false) => "Integer"
      case IntMultiType(false, Width2, _) => "Integer"
      case IntMultiType(false, Width4, _) => "Long"
      case IntMultiType(false, Width8, _) => "Long"

      case Int1Type(true) => "Byte"
      case IntMultiType(true, Width2, _) => "Short"
      case IntMultiType(true, Width4, _) => "Integer"
      case IntMultiType(true, Width8, _) => "Long"

      case CalcIntType => "Integer"
      case CalcFloatType => "Double"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case t: UserType => type2class(t.name.last)
      case EnumType(name, _) => type2class(name)

      case ArrayType(inType) => s"ArrayList<${kaitaiType2JavaTypeBoxed(inType)}>"
    }
  }

  def types2class(names: List[String]) = names.map {
    case "kaitai_struct" => kstructName
    case x => type2class(x)
  }.mkString(".")
}
