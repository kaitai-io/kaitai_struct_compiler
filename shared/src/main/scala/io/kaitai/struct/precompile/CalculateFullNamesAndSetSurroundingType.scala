package io.kaitai.struct.precompile

import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import io.kaitai.struct.problems.CompilationProblem

/**
  * Assigns a full name to the `.name` property to each KS type and enum.
  * Also for each [[ClassSpec]] assigns [[ClassSpec.upClass]] to a surrounding type.
  */
class CalculateFullNamesAndSetSurroundingType(classSpecs: ClassSpecs) extends PrecompileStep {
  override def run(): Iterable[CompilationProblem] = {
    classSpecs.foreach { case (_, curClass) => calculate(curClass) }
    None
  }

  private def calculate(curClass: ClassSpec): Unit = {
    curClass.enums.foreach { case (enumName, enumSpec) =>
      enumSpec.name = curClass.name ::: List(enumName)
    }

    curClass.types.foreach { case (nestedName: String, nestedClass) =>
      nestedClass.name = curClass.name ::: List(nestedName)
      nestedClass.upClass = Some(curClass)
      calculate(nestedClass)
    }
  }
}
