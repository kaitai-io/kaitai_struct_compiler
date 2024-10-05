package io.kaitai.struct.precompile

import io.kaitai.struct.{ClassTypeProvider, Log}
import io.kaitai.struct.format.{ClassSpec, ClassSpecs, ValueInstanceSpec}
import io.kaitai.struct.problems.{CompilationProblem, ErrorInInput}
import io.kaitai.struct.translators.TypeDetector

/**
  * Assign types to value instances by deriving them from their expressions.
  *
  * Calculates value of the [[ValueInstanceSpec.dataTypeOpt]] field, which is
  * a type of value instance.
  */
class DeriveValueInstanceTypes(specs: ClassSpecs) extends PrecompileStep {
  override def run(): Iterable[CompilationProblem] = {
    var iterNum = 1
    var hasChanged = false
    do {
      hasChanged = false
      Log.typeProcValue.info(() => s"### DeriveValueInstanceTypes: iteration #$iterNum")
      specs.foreach { case (specName, spec) =>
        Log.typeProcValue.info(() => s"#### $specName")

        val provider = new ClassTypeProvider(specs, spec)
        val detector = new TypeDetector(provider)

        val thisChanged = deriveValueType(spec, provider, detector)
        Log.typeProcValue.info(() => ".... => " + (if (thisChanged) "changed" else "no changes"))
        hasChanged |= thisChanged
      }
      iterNum += 1
    } while (hasChanged)
    Log.typeProcValue.info(() => s"## value type deriving finished in ${iterNum - 1} iteration(s)")
    None
  }

  private def deriveValueType(
    curClass: ClassSpec,
    provider: ClassTypeProvider,
    detector: TypeDetector
  ): Boolean = {
    Log.typeProcValue.info(() => s"deriveValueType(${curClass.nameAsStr})")
    var hasChanged = false

    provider.nowClass = curClass;
    curClass.instances.foreach {
      case (instName, inst) =>
        inst match {
          case vi: ValueInstanceSpec =>
            vi.dataTypeOpt match {
              case None =>
                try {
                  val viType = detector.detectType(vi.value)
                  vi.dataTypeOpt = Some(viType)
                  Log.typeProcValue.info(() => s"${instName.name} derived type: $viType")
                  hasChanged = true
                } catch {
                  case tue: TypeUndecidedError =>
                    Log.typeProcValue.info(() => s"${instName.name} type undecided: ${tue.getMessage}")
                    // just ignore, we're not there yet, probably we'll get it on next iteration
                  case err: ExpressionError =>
                    throw ErrorInInput(err, vi.path ++ List("value")).toException
                }
              case Some(_) =>
                // already derived, do nothing
            }
          case _ =>
            // do nothing
        }
    }

    // Continue with all nested types
    curClass.types.foreach {
      case (_, classSpec) =>
        hasChanged ||= deriveValueType(classSpec, provider, detector)
    }

    hasChanged
  }
}
