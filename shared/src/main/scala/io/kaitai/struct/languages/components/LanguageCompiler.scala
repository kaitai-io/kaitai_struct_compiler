package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.{DataType, Endianness, FixedEndian, InheritedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.translators.AbstractTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig}

import scala.collection.mutable.ListBuffer

abstract class LanguageCompiler(
  val typeProvider: ClassTypeProvider,
  val config: RuntimeConfig
) extends SwitchOps with ValidateOps
  with ExtraAttrs {

  val translator: AbstractTranslator

  /**
    * @return compilation results as a map: keys are file names, values are
    *         file contents.
    */
  def results(topClass: ClassSpec): Map[String, String]

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

  /**
    * Determines whether the language needs docstrings to be generated
    * inside classes and methods (true, Python-style) or outside them
    * (false, JavaDoc-style, majority of other languages). Affects calling
    * sequence of rendering methods.
    *
    * @return true if language needs docstrings to be generated
    * inside classes and methods, false otherwise
    */
  def innerDocstrings: Boolean = false

  def debug: Boolean = !config.autoRead && config.readStoresPos

  /**
    * @return String which is supposed to fill one level of indentation customary to the target
    *         language. Most often it's either a tab or a sequence of spaces.
    */
  def indent: String

  def type2class(className: String): String

  /**
    * Generates file header for a top-level type.
    * @param topClassName top-level name type in KS notation (lower underscore)
    */
  def fileHeader(topClassName: String): Unit

  /**
    * Generated file footer for a top-level type.
    * @param topClassName top-level name type in KS notation (lower underscore)
    */
  def fileFooter(topClassName: String): Unit = {}
  def importFile(file: String): Unit = {}

  /**
    * Outputs declaration of "opaque class", i.e. class that will be referred to in this file, but
    * not declared here. Some languages require either a "forward declaration" in this case, or a
    * statement to import that class, or something similar. Called once per each opaque class.
    * @param classSpec
    */
  def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {}

  def classDoc(name: List[String], doc: DocSpec): Unit = {}
  def classHeader(name: List[String]): Unit
  def classFooter(name: List[String]): Unit
  def classForwardDeclaration(name: List[String]): Unit = {}

  def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit
  def classConstructorFooter: Unit

  def classDestructorHeader(name: List[String], parentType: DataType, topClassName: List[String]): Unit = {}
  def classDestructorFooter: Unit = {}

  def runRead(name: List[String]): Unit
  def runReadCalc(): Unit

  /**
    * Generates header for a common `_read()` method or an endianness-specific `_read_be()` /
    * `_read_le()` method - typically a method declaration/definition. This method is supposed to
    * perform reading of sequence attributes.
    * @param endian specifies whether to create a common `_read` method (if None), or an
    *               endianness-specific `read_be` or `read_le` method (if Some)
    * @param isEmpty true if created method is expected to be empty, i.e. no sequence attributes
    *                reads are expected to be performed inside it.
    */
  def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit

  /**
    * Generates footer for a `_read()` / `_read_be()` / `_read_le()` methods. This method is
    * supposed to perform reading of sequence attributes.
    */
  def readFooter(): Unit

  def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit
  def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit
  def attributeDoc(id: Identifier, doc: DocSpec): Unit = {}

  def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit
  def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit
  def attrInit(attr: AttrLikeSpec): Unit = {}
  def attrDestructor(attr: AttrLikeSpec, id: Identifier): Unit = {}

  // TODO: delete
  def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit

  def condIfSetNull(instName: Identifier): Unit = {}
  def condIfSetNonNull(instName: Identifier): Unit = {}
  def condIfHeader(expr: Ast.expr): Unit
  def condIfFooter(expr: Ast.expr): Unit

  def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit

  def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit
  def condRepeatEosFooter: Unit

  def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit
  def condRepeatExprFooter: Unit

  def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit
  def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit

  def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit

  def normalIO: String
  def useIO(ioEx: Ast.expr): String
  def pushPos(io: String): Unit
  def seek(io: String, pos: Ast.expr): Unit
  def popPos(io: String): Unit
  def alignToByte(io: String): Unit

  def instanceDeclHeader(className: List[String]): Unit = {}
  def instanceClear(instName: InstanceIdentifier): Unit = {}
  def instanceSetCalculated(instName: InstanceIdentifier): Unit = {}
  def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = attributeDeclaration(attrName, attrType, isNullable)
  def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit
  def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit
  def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr)

  def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit

  /**
    * Outputs class' attributes sequence identifiers as some sort of an ordered sequence,
    * for debugging purposes. Useful for languages which do not retain fields order of
    * declaration in reflection.
    * @param seq sequence of attributes in a class
    */
  def debugClassSequence(seq: List[AttrSpec]) = {}

  /**
    * Generates custom member of the class (typically named `toString()` / `inspect` /
    * `__repr__` / similar) that gets contents of all important members of the class
    * as a single string. Usually used for debugging purposes / internal dumping mechanism.
    * Custom expression to render can be specified with `to-string` type-level KSY key.
    * @param toStringExpr custom expression in class context to render the string.
    */
  def classToString(toStringExpr: Ast.expr): Unit = {}

  def attrParseIfHeader(id: Identifier, ifExpr: Option[Ast.expr]): Unit = {
    ifExpr match {
      case Some(e) =>
        condIfSetNull(id)
        condIfHeader(e)
        condIfSetNonNull(id)
      case None => // ignore
    }
  }

  def attrParseIfFooter(ifExpr: Option[Ast.expr]): Unit = {
    ifExpr match {
      case Some(e) => condIfFooter(e)
      case None => // ignore
    }
  }

  def blockScopeHeader: Unit = {}
  def blockScopeFooter: Unit = {}
}
