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

  def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit =
    classConstructorHeader(name.last, parentType, rootClassName.last, isHybrid, params)
  def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit

  def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit =
    instanceHeader(className.last, instName, dataType, isNullable)
  def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit

  def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit =
    enumDeclaration(curClass.last, enumName, enumColl.map((x) => (x._1, x._2.name)))
  def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit
}
