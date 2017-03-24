package io.kaitai.struct.precompile

import io.kaitai.struct.format.ClassSpec

object MarkupClassNames {
  def markupClassNames(curClass: ClassSpec): Unit = {
    curClass.enums.foreach { case (enumName, enumSpec) =>
      enumSpec.name = curClass.name ::: List(enumName)
    }

    curClass.types.foreach { case (nestedName: String, nestedClass) =>
      nestedClass.name = curClass.name ::: List(nestedName)
      nestedClass.upClass = Some(curClass)
      markupClassNames(nestedClass)
    }
  }
}
