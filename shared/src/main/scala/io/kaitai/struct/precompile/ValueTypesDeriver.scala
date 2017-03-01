package io.kaitai.struct.precompile

import io.kaitai.struct.format.{ClassSpec, ValueInstanceSpec, YAMLParseException}
import io.kaitai.struct.translators.TypeDetector
import io.kaitai.struct.{ClassTypeProvider, Log}

class ValueTypesDeriver(topClass: ClassSpec) {
  val provider = new ClassTypeProvider(topClass)
  val detector = new TypeDetector(provider)

  def run() {
    var iterNum = 1
    var hasChanged = false
    do {
      Log.typeProcValue.info(() => s"### deriveValueType: iteration #$iterNum")
      hasChanged = deriveValueType(topClass, List())
      iterNum += 1
    } while (hasChanged)
  }

  def deriveValueType(curClass: ClassSpec, path: List[String]): Boolean = {
    Log.typeProcValue.info(() => s"deriveValueType(${curClass.nameAsStr})")
    var hasChanged = false

    provider.nowClass = curClass
    curClass.instances.foreach {
      case (instName, inst) =>
        inst match {
          case vi: ValueInstanceSpec =>
            vi.dataType match {
              case None =>
                try {
                  val viType = detector.detectType(vi.value)
                  vi.dataType = Some(viType)
                  Log.typeProcValue.info(() => s"${instName.name} derived type: $viType")
                  hasChanged = true
                } catch {
                  case tue: TypeUndecidedError =>
                    Log.typeProcValue.info(() => s"${instName.name} type undecided: ${tue.getMessage}")
                    // just ignore, we're not there yet, probably we'll get it on next iteration
                  case err: ExpressionError =>
                    throw new YAMLParseException(
                      err.getMessage,
                      path ++ List("instances", instName.name, "value")
                    )
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
      case (typeName, classSpec) =>
        hasChanged ||= deriveValueType(classSpec, path ++ List("types", typeName))
    }

    hasChanged
  }
}
