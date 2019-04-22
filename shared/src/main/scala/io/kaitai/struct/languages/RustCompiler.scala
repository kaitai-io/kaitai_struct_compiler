package io.kaitai.struct.languages

import io.kaitai.struct.datatype.{DataType, Endianness, FixedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{RepeatSpec, _}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.RustTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig}

class RustCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalFooter
    with UniversalDoc {

  override val translator = new RustTranslator(typeProvider, config)

  override def indent: String = "    "

  override def outFileName(topClassName: String): String = s"$topClassName.rs"

  override def outImports(topClass: ClassSpec): String =
    // TODO: #![allow(unused_imports)] instead?
    importList.toList.map(i => s"#[allow(unused_imports)]\nuse $i;").mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    // Runtime-required imports
    importList.add("kaitai_runtime::{self, KaitaiError, KaitaiStream, KaitaiStruct}")
    importList.add("std::vec::Vec")
  }

  override def classHeader(name: List[String]): Unit = out.puts(s"// classHeader($name)")

  override def classFooter(name: List[String]): Unit = out.puts(s"// classFooter($name)")

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String],
                                      isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    out.puts(s"// classConstructorHeader($name, $parentType, $rootClassName, $isHybrid, $params)")
  }

  override def runRead(): Unit = out.puts("// runRead()")

  override def runReadCalc(): Unit = out.puts("// runReadCalc()")

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = out.puts(s"// readHeader($endian, $isEmpty)")

  override def readFooter(): Unit = out.puts(s"// readFooter()")

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit =
    out.puts(s"// attributeDeclaration($attrName, $attrType, $isNullable)")

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit =
    out.puts(s"// attributeReader($attrName, $attrType, $isNullable)")

  override def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit =
    out.puts(s"// attrParse($attr, $id, $defEndian)")

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit =
    out.puts(s"// attrParseHybrid(${leProc()}, ${beProc()})")

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit =
    out.puts(s"// attrFixedContentsParse($attrName, $contents)")

  override def condIfHeader(expr: Ast.expr): Unit =
    out.puts(s"// condIfHeader($expr)")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit =
    out.puts(s"// condRepeatEosHeader($id, $io, $dataType, $needRaw)")

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit =
    out.puts(s"// condRepeatExprHeader($id, $io, $dataType, $needRaw, $repeatExpr)")

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit =
    out.puts(s"// condRepeatUntilHeader($id, $io, $dataType, $needRaw, $repeatExpr)")

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit =
    out.puts(s"// condRepeatUntilFooter($id, $io, $dataType, $needRaw, $repeatExpr)")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit =
    out.puts(s"// attrProcess($proc, $varSrc, $varDest)")

  override def useIO(ioEx: Ast.expr): String = s"// useIO($ioEx)"

  override def pushPos(io: String): Unit = out.puts(s"// pushPos($io)")

  override def seek(io: String, pos: Ast.expr): Unit = out.puts(s"// seek($io, $pos)")

  override def popPos(io: String): Unit = out.puts(s"// popPos($io)")

  override def alignToByte(io: String): Unit = out.puts(s"// alignToByte($io)")

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit =
    out.puts(s"// instanceHeader($className, $instName, $dataType, $isNullable)")

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit =
    out.puts(s"// instanceCheckCacheAndReturn($instName, $dataType)")

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit =
    out.puts(s"// instanceReturn($instName, $attrType)")

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit =
    out.puts(s"// instanceCalculate($instName, $dataType, $value)")

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit =
    out.puts(s"// enumDeclaration($curClass, $enumName, $enumColl)")

  /**
    * Renders identifier to a string, specifically for a given
    * language and settings. This usually includes things like
    * case and separator conversion and does *not* include things
    * like prepending "@" or "this." or "self." that might be
    * used to access private member.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def idToStr(id: Identifier): String = s"// idToStr(id)"

  /**
    * Renders identifier as a proper reference to a private member
    * that represents this field. This might include some prefixes
    * like "@" or "this." or "self.".
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def privateMemberName(id: Identifier): String = s"// privateMemberName($id)"

  /**
    * Renders identifier as a proper reference to a public member
    * that represents this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def publicMemberName(id: Identifier): String = s"// publicMemberName($id)"

  /**
    * Renders identifier as a proper reference to a local temporary
    * variable appropriately named to hold a temporary reference to
    * this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def localTemporaryName(id: Identifier): String = s"// localTemporaryName($id)"

  /**
    * Single method that outputs all kind of footers in the language.
    */
  override def universalFooter: Unit = {
    out.puts("// universalFooter()")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    doc.summary.foreach(out.putsLines("/// ", _))
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = out.puts(s"// switchStart($id, $on)")

  override def switchCaseStart(condition: Ast.expr): Unit = out.puts(s"// switchCaseStart($condition)")

  override def switchCaseEnd(): Unit = out.puts(s"// switchCaseEnd()")

  override def switchElseStart(): Unit = out.puts(s"// switchElseStart()")

  override def switchEnd(): Unit = out.puts(s"// switchEnd()")

  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = {
    out.puts(s"// extraAttrForIO($id, $rep)")
    Nil
  }
}

object RustCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses {

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new RustCompiler(tp, config)
}
