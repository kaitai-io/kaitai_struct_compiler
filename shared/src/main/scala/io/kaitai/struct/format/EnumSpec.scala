package io.kaitai.struct.format

import io.kaitai.struct.problems.KSYParseError

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.DataType.IntType
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.BitsType1
import io.kaitai.struct.datatype.DataType.BitsType
import scala.util.control.NonFatal

case class EnumSpec(
  path: List[String],
  doc: DocSpec,
  intType: IntType,
  map: SortedMap[BigInt, EnumValueSpec]
) extends YAMLPath {
  var name = List[String]()

  /**
    * @return Absolute name of enum as string, components separated by
    *         double colon operator `::`
    */
  def nameAsStr = name.mkString("::")

  /**
    * Determines whether this `EnumSpec` represents an enum that is external
    * (i.e. not defined in the same .ksy file) from the perspective of the given
    * `ClassSpec`.
    * @param curClass class spec from which the local/external relationship
    * should be evaluated
    */
  def isExternal(curClass: ClassSpec): Boolean =
    name.head != curClass.name.head
}

object EnumSpec {
  val LEGAL_KEYS = Set(
    "doc",
    "doc-ref",
    "type",
    "values"
  )

  def fromYaml(src: Any, path: List[String]): EnumSpec = {
    val srcMap = ParseUtils.asMap(src, path)
    // Check whether we're dealing with the old enum syntax used in KS 0.11 and
    // earlier. Strictly speaking, this is not necessary (the old syntax would
    // be rejected anyway), but we do this to make the error message as helpful
    // as possible.
    if (srcMap.nonEmpty && !LEGAL_KEYS.exists(srcMap.contains) && srcMap.exists { case (key, _) =>
      try {
        ParseUtils.asBigInt(key, Nil)
        true
      } catch {
        case NonFatal(_) => false
      }
    }) {
      throw KSYParseError.withText(
        "legacy pre-v0.12 enum syntax; add `type: <int_type>` (e.g. `type: u4`) and indent entries under the `values` key",
        path
      )
    }

    // At this point, either the map is empty or contains no integer keys (in
    // which case we will report missing mandatory properties of the new
    // syntax), or an attempt was made to use the new KS 0.12+ syntax (see
    // https://github.com/kaitai-io/kaitai_struct/issues/1288), so we are
    // treating it as such
    val srcMapStr = ParseUtils.anyMapToStrMap(srcMap, path)
    ParseUtils.ensureLegalKeys(srcMapStr, LEGAL_KEYS, path)

    val typeStr = ParseUtils.getValueStr(srcMapStr, "type", path)
    val valuesMap = ParseUtils.getValueMap(srcMapStr, "values", path)

    val dataType = DataType.pureFromString(Some(typeStr))
    val intType = dataType match {
      case it: IntType => it
      // `type: b1` is automatically mapped to a boolean by
      // DataType.pureFromString(). We don't want that here, so we convert it
      // back to a 1-bit integer.
      case BitsType1(bitEndian) =>
        BitsType(1, bitEndian)
      case other =>
        throw KSYParseError.withText(
          s"expected an integer type with no endianness (i.e. `uX` / `sX` / `bX`), got `${typeStr}`",
          path :+ "type"
        )
    }

    val doc = DocSpec.fromYaml(srcMapStr, path)

    val memberNameMap = mutable.Map[String, BigInt]()
    val valuesPath = path :+ "values"

    EnumSpec(path, doc, intType, SortedMap.from(
      valuesMap.map { case (id, desc) =>
        val idBigInt = ParseUtils.asBigInt(id, valuesPath)
        val value = EnumValueSpec.fromYaml(desc, valuesPath :+ idBigInt.toString)
        memberNameMap.get(value.name).foreach { (prevIdBigInt) =>
          throw KSYParseError.withText(
            s"duplicate enum member ID: '${value.name}', previously defined at /${(valuesPath :+ prevIdBigInt.toString).mkString("/")}",
            valuesPath :+ idBigInt.toString
          )
        }
        memberNameMap.put(value.name, idBigInt)
        idBigInt -> value
      }
    ))
  }
}
