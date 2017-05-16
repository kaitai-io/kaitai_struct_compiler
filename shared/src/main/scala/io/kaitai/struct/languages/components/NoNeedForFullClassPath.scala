package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.format._

trait NoNeedForFullClassPath {
  def classHeader(name: List[String]): Unit =
    classHeader(name.last)
  def classHeader(name: String): Unit

  def classFooter(name: List[String]): Unit =
    classFooter(name.last)
  def classFooter(name: String): Unit

  def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean): Unit =
    classConstructorHeader(name.last, parentType, rootClassName.last, isHybrid)
  def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean): Unit

  def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType): Unit =
    instanceHeader(className.last, instName, dataType)
  def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType): Unit

  def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, String)]): Unit =
    enumDeclaration(curClass.last, enumName, enumColl)
  def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit
}
