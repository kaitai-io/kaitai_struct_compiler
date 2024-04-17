package io.kaitai.struct.datatype

import io.kaitai.struct.datatype.DataType.SwitchType
import io.kaitai.struct.exprlang.Expressions
import io.kaitai.struct.format.ClassSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.SortedMap

class SwitchType$Test extends AnyFunSpec {
  describe("SwitchType.parseSwitch") {
    it ("combines ints properly") {
      val t = SwitchType(
        Expressions.parse("foo"),
        SortedMap(
          Expressions.parse("1") -> DataType.IntMultiType(true, DataType.Width2, Some(LittleEndian)),
          Expressions.parse("2") -> DataType.IntMultiType(false, DataType.Width4, Some(LittleEndian))
        )
      )

      t.combinedType should be(DataType.CalcIntType)
    }

    it ("combines owning user types properly") {
      val ut1 = DataType.UserTypeInstream(List("foo"), None)
      ut1.classSpec = Some(ClassSpec.opaquePlaceholder(List("foo")))
      val ut2 = DataType.UserTypeInstream(List("bar"), None)
//      ut2.classSpec = Some(ClassSpec.opaquePlaceholder(List("bar")))

      val t = SwitchType(
        Expressions.parse("foo"),
        SortedMap(
          Expressions.parse("1") -> ut1,
          Expressions.parse("2") -> ut2
        )
      )

      t.combinedType should be(DataType.KaitaiStructType)
    }
  }
}
