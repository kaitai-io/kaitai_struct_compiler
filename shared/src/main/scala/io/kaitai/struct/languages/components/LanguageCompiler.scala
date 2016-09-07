package io.kaitai.struct.languages.components

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType.BaseType
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, TypeProvider}

import scala.collection.mutable.ListBuffer

abstract class LanguageCompiler(verbose: Boolean, out: LanguageOutputWriter) {
  /**
    * Declares whether language is capable of doing inner classes (i.e. classes
    * nested inside each other) or not. Affects calling sequence of rendering
    * methods.
    * @return whether language is capable of doing inner classes
    */
  def innerClasses: Boolean = true

  protected var _translator: Option[BaseTranslator] = None
  protected var _currentIteratorType: Option[BaseType] = None
  protected var _currentSwitchType: Option[BaseType] = None

  def getStatic: LanguageCompilerStatic
  def open(topClassName: String, tp: TypeProvider): Unit = {
    _translator = Some(getStatic.getTranslator(tp))
  }
  def close = out.close
  def translator: BaseTranslator = _translator.get
  def currentIteratorType: BaseType = _currentIteratorType.get
  def currentSwitchType: BaseType = _currentSwitchType.get

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

  def condRepeatUntilHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: Ast.expr): Unit
  def condRepeatUntilFooter(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: Ast.expr): Unit

  def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit

  def normalIO: String
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
}
