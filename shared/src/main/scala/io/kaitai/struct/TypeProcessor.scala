package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._

import scala.collection.mutable

object TypeProcessor {
  def getExternalTypes(curClass: ClassSpec): Iterable[List[String]] = {
    val res = mutable.Set[List[String]]()
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

  def getExternalTypesFromDataType(dataType: DataType, curClass: ClassSpec): Iterable[List[String]] = {
    dataType match {
      case ut: UserType =>
        if (ut.isExternal(curClass)) {
          List(ut.classSpec.get.name)
        } else {
          List()
        }
      case et: EnumType =>
        if (et.isExternal(curClass)) {
          List(et.enumSpec.get.name)
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
