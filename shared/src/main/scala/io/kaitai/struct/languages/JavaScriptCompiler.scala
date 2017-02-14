package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{BaseTranslator, JavaScriptTranslator, TypeProvider}
import io.kaitai.struct.{LanguageOutputWriter, RuntimeConfig, Utils}

import scala.collection.mutable.ListBuffer

class JavaScriptCompiler(config: RuntimeConfig, out: LanguageOutputWriter)
  extends LanguageCompiler(config, out)
    with ObjectOrientedLanguage
    with UniversalDoc
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FixedContentsUsingArrayByteLiteral {
  import JavaScriptCompiler._

  override def getStatic = JavaScriptCompiler

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"// $headerComment")
  }

  override def fileFooter(name: String): Unit = {
    out.puts
    out.puts("// Export for amd environments")
    out.puts("if (typeof define === 'function' && define.amd) {")
    out.inc
    out.puts(s"define('${type2class(name)}', [], function() {")
    out.inc
    out.puts(s"return ${type2class(name)};")
    out.dec
    out.puts("});")
    out.dec
    out.puts("}")

    out.puts

    out.puts("// Export for CommonJS")
    out.puts("if (typeof module === 'object' && module && module.exports) {")
    out.inc
    out.puts(s"module.exports = ${type2class(name)};")
    out.dec
    out.puts("}")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val typeName = classSpec.name.head
    out.puts
    out.puts("if (typeof require === 'function')")
    out.inc
    out.puts(s"var ${type2class(typeName)} = require('./${outFileName(typeName)}');")
    out.dec
  }

  override def classHeader(name: List[String]): Unit = {
    val shortClassName = type2class(name.last)

    val addNameExpr = if (name.size > 1) {
      s" = ${types2class(name.takeRight(2))}"
    } else {
      ""
    }

    out.puts
    out.puts(s"var $shortClassName$addNameExpr = (function() {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    out.puts
    out.puts(s"return ${type2class(name.last)};")
    out.dec
    out.puts("})();")
  }

  override def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit = {
    out.puts(s"function ${type2class(name.last)}(_io, _parent, _root) {")
    out.inc
    out.puts("this._io = _io;")
    out.puts("this._parent = _parent;")
    out.puts("this._root = _root || this;")
    if (debug) {
      out.puts("this._debug = {};")
      out.dec
      out.puts("}")
      out.puts
      out.puts(s"${type2class(name.last)}.prototype._read = function() {")
      out.inc
    } else {
      out.puts
    }
  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def universalDoc(doc: String): Unit = {
    // JSDoc docstring style: http://usejsdoc.org/about-getting-started.html
    out.puts
    out.puts( "/**")
    out.putsLines(" * ", doc)
    out.puts( " */")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {
    out.puts(s"${privateMemberName(attrName)} = " +
      s"$normalIO.ensureFixedContents($contents);")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        out.puts(s"$destName = $kstreamName.$procName($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$destName = $kstreamName.processZlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName.processRotateLeft($srcName, $expr, 1);")
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val langName = idToStr(varName)
    val memberCall = privateMemberName(varName)

    val ioName = s"_io_$langName"

    val args = rep match {
      case RepeatEos => s"$memberCall[$memberCall.length - 1]"
      case RepeatExpr(_) => s"$memberCall[i]"
      case NoRepeat => memberCall
    }

    out.puts(s"var $ioName = new $kstreamName($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"var io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"var _pos = $io.pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.alignToByte();")

  override def attrDebugStart(attrId: Identifier, attrType: BaseType, io: Option[String], rep: RepeatSpec): Unit = {
    if (!attrDebugNeeded(attrId))
      return

    val debugName = attrDebugName(attrId, rep, false)

    val ioProps = io match {
      case None => ""
      case Some(x) => s"start: $x.pos, ioOffset: $x._byteOffset"
    }

    val enumNameProps = attrType match {
      case t: EnumType => s"""enumName: \"${types2class(t.enumSpec.get.name)}\""""
      case _ => ""
    }

    out.puts(s"$debugName = { $ioProps${if (ioProps != "" && enumNameProps != "") ", " else ""}$enumNameProps };")
  }

  override def attrDebugEnd(attrId: Identifier, attrType: BaseType, io: String, rep: RepeatSpec): Unit = {
    if (!attrDebugNeeded(attrId))
      return
    val debugName = attrDebugName(attrId, rep, true)

    out.puts(s"$debugName.end = $io.pos;")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    if (debug)
      out.puts(s"this._debug.${idToStr(id)}.arr = [];")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new Array(${expression(repeatExpr)});")
    out.puts(s"${privateMemberName(id)} = new Array(${expression(repeatExpr)});")
    if (debug)
      out.puts(s"this._debug.${idToStr(id)}.arr = new Array(${expression(repeatExpr)});")
    out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[i] = $expr;")
  }

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    if (debug)
      out.puts(s"this._debug.${idToStr(id)}.arr = [];")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String): Unit = {
    out.puts(s"var ${translator.doName("_")} = $expr;")
    out.puts(s"${privateMemberName(id)}.push(${translator.doName("_")});")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def handleAssignmentTempVar(dataType: BaseType, id: String, expr: String): Unit =
    out.puts(s"var $id = $expr;")

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall)}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io.readBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io.readBitsInt($width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) "" else ", this, this._root"
        s"new ${type2class(t.name.last)}($io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String): Unit = {
    out.puts(s"$id._read();")
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"switch (${expression(on)}) {")

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

  override def switchEnd(): Unit =
    out.puts("}")

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: BaseType): Unit = {
    out.puts(s"Object.defineProperty(${type2class(className.last)}.prototype, '${publicMemberName(instName)}', {")
    out.inc
    out.puts("get: function() {")
    out.inc
  }

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("});")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== undefined)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    out.puts(s"${type2class(curClass.last)}.${type2class(enumName)} = Object.freeze({")
    out.inc
    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumValue(enumName, label)}: $id,")
    }
    out.puts
    enumColl.foreach { case (id, label) =>
      out.puts(s"""$id: "${enumValue(enumName, label)}",""")
    }
    out.dec
    out.puts("});")
    out.puts
  }

  def enumValue(enumName: String, label: String) = label.toUpperCase

  override def debugClassSequence(seq: List[AttrSpec]) = {
    //val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    //out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_${Utils.lowerCamelCase(name)}"
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  override def publicMemberName(id: Identifier): String = {
    id match {
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
    }
  }

  private
  def attrDebugNeeded(attrId: Identifier) = attrId match {
    case _: NamedIdentifier | _: NumberedIdentifier | _: InstanceIdentifier => true
    case _: RawIdentifier | _: SpecialIdentifier => false
  }

  def attrDebugName(attrId: Identifier, rep: RepeatSpec, end: Boolean) = {
    val arrIndexExpr = rep match {
      case NoRepeat => ""
      case _: RepeatExpr => ".arr[i]"
      case RepeatEos | _: RepeatUntil => s".arr[${privateMemberName(attrId)}.length${if (end) " - 1" else ""}]"
    }

    s"this._debug.${idToStr(attrId)}$arrIndexExpr"
  }
}

object JavaScriptCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaScriptTranslator(tp)
  override def indent: String = "  "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.js"

  override def kstreamName: String = "KaitaiStream"

  // FIXME: probably KaitaiStruct will emerge some day in JavaScript runtime, but for now it is unused
  override def kstructName: String = ???

  def types2class(types: List[String]): String =
    types.map(JavaScriptCompiler.type2class).mkString(".")
}
