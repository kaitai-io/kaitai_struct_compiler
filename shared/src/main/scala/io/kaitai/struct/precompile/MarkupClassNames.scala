package io.kaitai.struct.precompile

import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import io.kaitai.struct.problems.CompilationProblem

class MarkupClassNames(classSpecs: ClassSpecs) extends PrecompileStep {
  override def run(): Iterable[CompilationProblem] = {
    classSpecs.foreach { case (_, curClass) => markupClassNames(curClass) }
    None
  }

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
