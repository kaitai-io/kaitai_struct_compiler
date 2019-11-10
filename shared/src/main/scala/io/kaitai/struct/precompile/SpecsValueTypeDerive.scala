package io.kaitai.struct.precompile

import io.kaitai.struct.Log
import io.kaitai.struct.format.ClassSpecs

class SpecsValueTypeDerive(specs: ClassSpecs) {
  def run(): Unit = {
    var iterNum = 1
    var hasChanged = false
    do {
      hasChanged = false
      Log.typeProcValue.info(() => s"### SpecsValueTypeDerive: iteration #$iterNum")
      specs.foreach { case (specName, spec) =>
        Log.typeProcValue.info(() => s"#### $specName")
        val thisChanged = new ValueTypesDeriver(specs, spec).run()
        Log.typeProcValue.info(() => ".... => " + (if (thisChanged) "changed" else "no changes"))
        hasChanged |= thisChanged
      }
      iterNum += 1
    } while (hasChanged)
    Log.typeProcValue.info(() => s"## value type deriving finished in ${iterNum - 1} iteration(s)")
  }
}
