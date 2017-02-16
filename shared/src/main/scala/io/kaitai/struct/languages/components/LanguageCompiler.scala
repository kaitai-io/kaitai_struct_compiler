package io.kaitai.struct.languages.components

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType.{BaseType, BooleanType}
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, TypeMismatchError}
import io.kaitai.struct.{ClassTypeProvider, LanguageOutputWriter, RuntimeConfig}

import scala.collection.mutable.ListBuffer

abstract class LanguageCompiler(config: RuntimeConfig, out: LanguageOutputWriter) {
  /**
    * Declares whether language is capable of doing inner classes (i.e. classes
    * nested inside each other) or not. Affects calling sequence of rendering
    * methods.
    *
    * @return whether language is capable of doing inner classes
    */
  def innerClasses: Boolean = true

  /**
    * Declares whether language needs enums to be declared inside classes
    * (true) or outside them (false). Affects calling sequence of rendering
    * methods.
    *
    * @return whether language needs enums inside classes (true) or outside
    *         (false)
    */
  def innerEnums: Boolean = true

  protected var _translator: Option[BaseTranslator] = None
  protected var _typeProvider: Option[ClassTypeProvider] = None

  def getStatic: LanguageCompilerStatic
  def open(topClassName: String, tp: ClassTypeProvider): Unit = {
    _typeProvider = Some(tp)
    _translator = Some(getStatic.getTranslator(tp))
  }
  def close = out.close
  def typeProvider: ClassTypeProvider = _typeProvider.get
  def translator: BaseTranslator = _translator.get

  def debug = config.debug

  def fileHeader(topClassName: String): Unit
  def fileFooter(topClassName: String): Unit = {}

  /**
    * Outputs declaration of "opaque class", i.e. class that will be referred to in this file, but
    * not declared here. Some languages require either a "forward declaration" in this case, or a
    * statement to import that class, or something similar. Called once per each opaque class.
    * @param classSpec
    */
  def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {}

  def classDoc(name: List[String], doc: String): Unit = {}
  def classHeader(name: List[String]): Unit
  def classFooter(name: List[String]): Unit
  def classForwardDeclaration(name: List[String]): Unit = {}

  def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit
  def classConstructorFooter: Unit

  def classDestructorHeader(name: List[String], parentTypeName: List[String], topClassName: List[String]): Unit = {}
  def classDestructorFooter: Unit = {}

  def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit
  def attributeReader(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit
  def attributeDoc(id: Identifier, doc: String): Unit = {}

  def attrParse(attr: AttrLikeSpec, id: Identifier, extraAttrs: ListBuffer[AttrSpec]): Unit
  def attrDestructor(attr: AttrLikeSpec, id: Identifier): Unit = {}

  def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit

  def condIfSetNull(instName: Identifier): Unit = {}
  def condIfSetNonNull(instName: Identifier): Unit = {}
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
  def alignToByte(io: String): Unit

  def instanceClear(instName: InstanceIdentifier): Unit = {}
  def instanceSetCalculated(instName: InstanceIdentifier): Unit = {}
  def instanceDeclaration(attrName: InstanceIdentifier, attrType: BaseType, condSpec: ConditionalSpec) = attributeDeclaration(attrName, attrType, condSpec)
  def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: BaseType): Unit
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit
  def instanceReturn(instName: InstanceIdentifier): Unit
  def instanceCalculate(instName: InstanceIdentifier, dataType: BaseType, value: Ast.expr)

  def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, String)]): Unit

  /**
    * Outputs class' attributes sequence identifiers as some sort of an ordered sequence,
    * for debugging purposes. Useful for languages which do not retain fields order of
    * declaration in reflection.
    * @param seq sequence of attributes in a class
    */
  def debugClassSequence(seq: List[AttrSpec]) = {}

  def attrParseIfHeader(id: Identifier, ifExpr: Option[Ast.expr]): Unit = {
    ifExpr match {
      case Some(e) =>
        translator.detectType(e) match {
          case BooleanType =>
            // everything's fine, we've got boolean-resulting expression
            condIfSetNull(id)
            condIfHeader(e)
            condIfSetNonNull(id)
          case t =>
            throw new TypeMismatchError(s"expected boolean expression in `if`, got $t")
        }
      case None => // ignore
    }
  }

  def attrParseIfFooter(ifExpr: Option[Ast.expr]): Unit = {
    ifExpr match {
      case Some(e) => condIfFooter(e)
      case None => // ignore
    }
  }
}
