package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format.{NoRepeat, RepeatEos, RepeatExpr, RepeatSpec, _}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{BaseTranslator, PHPTranslator, TypeProvider}
import io.kaitai.struct.{ClassTypeProvider, LanguageOutputWriter, RuntimeConfig, Utils}

class PHPCompiler(config: RuntimeConfig, out: LanguageOutputWriter)
  extends LanguageCompiler(config, out)
    with ObjectOrientedLanguage
    with AllocateIOLocalVar
    with UniversalFooter
    with UniversalDoc
    with FixedContentsUsingArrayByteLiteral
    with EveryReadIsExpression {

  import PHPCompiler._

  override def innerClasses = false

  override def innerEnums = false

  override def getStatic = PHPCompiler

  // FIXME: special hack to provide an instance of PHPCompiler that knows of PHP namespace
  // to the translator
  override def open(topClassName: String, tp: ClassTypeProvider): Unit = {
    _typeProvider = Some(tp)
    _translator = Some(new PHPTranslator(tp, this))
  }

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def fileHeader(topClassName: String): Unit = {
    out.puts("<?php")
    out.puts(s"// $headerComment")
  }

  override def classHeader(name: List[String]): Unit =
    classHeader(name, Some(kstructName))

  def classHeader(name: List[String], parentClass: Option[String]): Unit = {
    val nsPart = name.dropRight(1)
    val ns = if (nsPart.nonEmpty) {
      config.phpNamespace + "\\" + types2classRel(nsPart)
    } else {
      config.phpNamespace
    }
    if (ns.nonEmpty) {
      out.puts
      out.puts(s"namespace $ns;")
    }
    out.puts

    val ext = parentClass match {
      case Some(x) => s" extends $x"
      case None => ""
    }

    out.puts(s"class ${type2class(name.last)}$ext {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = universalFooter

  override def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit = {
    out.puts
    out.puts(
      "public function __construct(" +
      kstreamName + " $io, " +
      types2classAbs(parentClassName) + " $parent = null, " +
      types2classAbs(rootClassName) + " $root = null) {"
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
      case ParentIdentifier | RootIdentifier | IoIdentifier =>
        // just ignore it for now
      case _ =>
        out.puts(s"protected $$_m_${idToStr(attrName)};")
    }
  }

  override def attributeReader(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    attrName match {
      case ParentIdentifier | RootIdentifier =>
        // just ignore it for now
      case _ =>
        out.puts(s"public function ${publicMemberName(attrName)}() { return ${privateMemberName(attrName)}; }")
    }
  }

  override def universalDoc(doc: String): Unit = {
    out.puts
    out.puts( "/**")
    out.putsLines(" * ", doc)
    out.puts( " */")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = $normalIO->ensureFixedContents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        out.puts(s"$destName = $kstreamName::$procName($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$destName = $kstreamName::processZlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName::processRotateLeft($srcName, $expr, 1);")
    }
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"end($memberName)"
      case NoRepeat => memberName
    }

    out.puts(s"$$io = new $kstreamName($args);")
    "$io"
  }

  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"$$io = ${expression(ioEx)};")
    "$io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"$$_pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io->seek($$_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io->alignToByte();")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts(s"while (!$io->isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[] = $expr;")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts(s"$$n = ${expression(repeatExpr)};")
    out.puts("for ($i = 0; $i < $n; $i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[] = $expr;")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String): Unit = {
    out.puts(s"${translator.doLocalName("_")} = $expr;")
    out.puts(s"${privateMemberName(id)}[] = ${translator.doLocalName("_")};")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read${Utils.capitalize(t.apiCall)}()"
      case blt: BytesLimitType =>
        s"$io->readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io->readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io->readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io->readBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io->readBitsInt($width)"
      case t: UserType =>
        val addArgs = if (!t.isOpaque) s", $$this, ${privateMemberName(RootIdentifier)}" else ""
        s"new ${types2classAbs(t.classSpec.get.name)}($io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName::bytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    val onType = translator.detectType(on)

    out.puts(s"switch (${expression(on)}) {")
    out.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"case ${expression(condition)}:")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
  }

  override def switchElseStart(): Unit = {
    out.puts("default:")
    out.inc
  }

  override def switchEnd(): Unit = universalFooter

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: BaseType): Unit = {
    out.puts(s"public function ${idToStr(instName)}() {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== null)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    classHeader(curClass ::: List(enumName), None)
    enumColl.foreach { case (id, label) =>
      out.puts(s"const ${value2Const(label)} = $id;")
    }
    universalFooter
  }

  def value2Const(label: String) = label.toUpperCase

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = {
    id match {
      case IoIdentifier => s"$$this->_io"
      case RootIdentifier => s"$$this->_root"
      case ParentIdentifier => s"$$this->_parent"
      case _ => s"$$this->_m_${idToStr(id)}"
    }
  }

  override def publicMemberName(id: Identifier) = idToStr(id)

  def namespaceRef = if (config.phpNamespace.isEmpty) {
    ""
  } else {
    "\\" + config.phpNamespace
  }

  def types2classAbs(names: List[String]) =
    names match {
      case List("kaitai_struct") => kstructName
      case _ =>
        namespaceRef + "\\" + types2classRel(names)
    }

  /**
    * Determine PHP data type corresponding to a KS data type. Currently unused due to
    * problems with nullable types (which were introduced only in PHP 7.1).
    *
    * @param attrType KS data type
    * @return PHP data type
    */
  def kaitaiType2NativeType(attrType: BaseType): String = {
    attrType match {
      case _: IntType => "int"
      case _: FloatType => "float"

      case BooleanType => "bool"

      case _: StrType | _: BytesType => "string"

      case t: UserType => types2classAbs(t.classSpec.get.name)
      case t: EnumType => "int"

      case ArrayType(_) => "array"
    }
  }
}

object PHPCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses {
  // FIXME: not really used, as we reimplement PHPCompiler.open()
  override def getTranslator(tp: TypeProvider): BaseTranslator = ???
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.php"

  override def kstreamName: String = "\\Kaitai\\Struct\\Stream"

  override def kstructName: String = "\\Kaitai\\Struct\\Struct"

  def types2classRel(names: List[String]) = names.map(type2class).mkString("\\")
}
