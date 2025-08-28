package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._

import scala.collection.mutable

sealed abstract class ExternalType {
  def name: List[String]
  def isOpaque: Boolean
}
case class ExternalUserType(classSpec: ClassSpec) extends ExternalType {
  def name: List[String] = classSpec.name
  def isOpaque: Boolean = classSpec.meta.isOpaque
}
case class ExternalEnum(enumSpec: EnumSpec) extends ExternalType {
  def name: List[String] = enumSpec.name
  def isOpaque: Boolean = false
}

object TypeProcessor {
  def getExternalTypes(curClass: ClassSpec): Iterable[ExternalType] = {
    val res = mutable.Set[ExternalType]()
    curClass.seq.foreach((attr) =>
      res ++= getExternalTypesFromDataType(attr.dataType, curClass)
    )
    curClass.params.foreach((param) =>
      res ++= getExternalTypesFromDataType(param.dataType, curClass)
    )
    curClass.instances.foreach { case (_, inst) =>
      inst match {
        case pis: ParseInstanceSpec =>
          res ++= getExternalTypesFromDataType(pis.dataType, curClass)
        case _ => None
      }
    }

    // Traverse all nested types recursively
    curClass.types.foreach { case (_, nestedType) =>
      res ++= getExternalTypes(nestedType)
    }

    res
  }

  def getExternalTypesFromDataType(dataType: DataType, curClass: ClassSpec): Iterable[ExternalType] = {
    dataType match {
      case ut: UserType =>
        if (ut.isExternal(curClass)) {
          List(ExternalUserType(ut.classSpec.get))
        } else {
          List()
        }
      case et: EnumType =>
        if (et.isExternal(curClass)) {
          List(ExternalEnum(et.enumSpec.get))
        } else {
          List()
        }
      case st: SwitchType =>
        st.cases.flatMap { case (_, ut) =>
          getExternalTypesFromDataType(ut, curClass)
        }
      case at: ArrayType =>
        getExternalTypesFromDataType(at.elType, curClass)
      case _ =>
        // all other types are not external user types
        List()
    }
  }
}
