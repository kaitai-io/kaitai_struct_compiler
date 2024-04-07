package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._

import scala.collection.mutable

object TypeProcessor {
  def getExternalClasses(curClass: ClassSpec): Iterable[ClassSpec] = {
    val res = mutable.Set[ClassSpec]()
    curClass.seq.map((attr) =>
      res ++= getExternalDataTypes(attr.dataType, curClass)
    )
    curClass.instances.foreach { case (_, inst) =>
      inst match {
        case pis: ParseInstanceSpec =>
          res ++= getExternalDataTypes(pis.dataType, curClass)
        case _ => None
      }
    }

    // Traverse all nested types recursively
    curClass.types.foreach { case (_, nestedType) =>
      res ++= getExternalClasses(nestedType)
    }

    res
  }

  def getExternalDataTypes(dataType: DataType, curClass: ClassSpec): Iterable[ClassSpec] = {
    dataType match {
      case ut: UserType =>
        if (ut.isExternal(curClass)) {
          List(ut.classSpec.get)
        } else {
          List()
        }
      case st: SwitchType =>
        st.cases.flatMap { case (_, ut) =>
          getExternalDataTypes(ut, curClass)
        }
      case _ =>
        // all other types are not external user types
        List()
    }
  }
}
