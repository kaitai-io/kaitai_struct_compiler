package io.kaitai.struct.languages.components

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType.BaseType
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, TypeProvider}

import scala.collection.mutable.ListBuffer

abstract class LanguageCompiler(verbose: Boolean, out: LanguageOutputWriter) {
  protected var _translator: Option[BaseTranslator] = None

  def getStatic: LanguageCompilerStatic
  def open(topClassName: String, tp: TypeProvider): Unit = {
    _translator = Some(getStatic.getTranslator(tp))
  }
  def close = out.close
  def translator: BaseTranslator = _translator.get

  def headerComment = "This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild"
  def fileHeader(topClassName: String): Unit
  def fileFooter(topClassName: String): Unit = {}

  def classHeader(name: List[String]): Unit
  def classFooter(name: List[String]): Unit
  def classForwardDeclaration(name: List[String]): Unit = {}

  def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit
  def classConstructorFooter: Unit

  def classDestructorHeader(name: List[String], parentTypeName: List[String], topClassName: List[String]): Unit = {}
  def classDestructorFooter: Unit = {}

  def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit
  def attributeReader(attrName: Identifier, attrType: BaseType): Unit

  def attrParse(attr: AttrLikeSpec, id: Identifier, extraAttrs: ListBuffer[AttrSpec], io: String): Unit
  def attrDestructor(attr: AttrLikeSpec, id: Identifier): Unit = {}

  def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit

  def condIfHeader(expr: Ast.expr): Unit
  def condIfFooter(expr: Ast.expr): Unit

  def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit
  def condRepeatEosFooter: Unit

  def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: Ast.expr): Unit
  def condRepeatExprFooter: Unit

  def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit

  def normalIO: String = privateMemberName(IoIdentifier)
  def useIO(ioEx: Ast.expr): String
  def pushPos(io: String): Unit
  def seek(io: String, pos: Ast.expr): Unit
  def popPos(io: String): Unit

  def instanceClear(instName: Identifier): Unit = {}
  def instanceSetCalculated(instName: Identifier): Unit = {}
  def instanceDeclaration(attrName: InstanceIdentifier, attrType: BaseType, condSpec: ConditionalSpec) = attributeDeclaration(attrName, attrType, condSpec)
  def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: BaseType): Unit
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit
  def instanceReturn(instName: InstanceIdentifier): Unit
  def instanceCalculate(instName: InstanceIdentifier, dataType: BaseType, value: Ast.expr)

  def enumDeclaration(curClass: List[String], enumName: String, enumColl: Map[Long, String]): Unit

  def expression(e: Ast.expr): String = translator.translate(e)

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
  def idToStr(id: Identifier): String

  /**
    * Renders identifier as a proper reference to a private member
    * that represents this field. This might include some prefixes
    * like "@" or "this." or "self.".
    *
    * @param id identifier to render
    * @return identifier as string
    */
  def privateMemberName(id: Identifier): String

  /**
    * Renders identifier as a proper reference to a public member
    * that represents this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  def publicMemberName(id: Identifier): String
}
