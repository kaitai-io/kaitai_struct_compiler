package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._

import scala.collection.mutable

object TypeProcessor {
  def getOpaqueClasses(curClass: ClassSpec): Iterable[ClassSpec] = {
    val res = mutable.Set[ClassSpec]()
    curClass.seq.map((attr) =>
      res ++= getOpaqueDataTypes(attr.dataType, curClass)
    )
    curClass.instances.foreach { case (_, inst) =>
      inst match {
        case pis: ParseInstanceSpec =>
          res ++= getOpaqueDataTypes(pis.dataType, curClass)
        case _ => None
      }
    }

    // Traverse all nested types recursively
    curClass.types.foreach { case (_, nestedType) =>
      res ++= getOpaqueClasses(nestedType)
    }

    res
  }

  def getOpaqueDataTypes(dataType: DataType, curClass: ClassSpec): Iterable[ClassSpec] = {
    dataType match {
      case ut: UserType =>
        if (ut.isExternal(curClass)) {
          List(ut.classSpec.get)
        } else {
          List()
        }
      case st: SwitchType =>
        st.cases.flatMap { case (_, ut) =>
          getOpaqueDataTypes(ut, curClass)
        }
      case _ =>
        // all other types are not opaque external user types
        List()
    }
  }
}
