package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.format.InstanceIdentifier

trait NoNeedForFullClassPath {
  def classHeader(name: List[String]): Unit =
    classHeader(name.last)
  def classHeader(name: String): Unit

  def classFooter(name: List[String]): Unit =
    classFooter(name.last)
  def classFooter(name: String): Unit

  def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit =
    classConstructorHeader(name.last, parentClassName.last, rootClassName.last)
  def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit

  def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType): Unit =
    instanceHeader(className.last, instName, dataType)
  def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType): Unit

  def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, String)]): Unit =
    enumDeclaration(curClass.last, enumName, enumColl)
  def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit
}
