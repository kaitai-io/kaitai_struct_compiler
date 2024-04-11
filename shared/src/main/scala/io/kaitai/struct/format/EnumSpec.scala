package io.kaitai.struct.format

import io.kaitai.struct.problems.KSYParseError

import scala.collection.immutable.SortedMap
import scala.collection.mutable

case class EnumSpec(path: List[String], map: SortedMap[Long, EnumValueSpec]) extends YAMLPath {
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
  def fromYaml(src: Any, path: List[String]): EnumSpec = {
    val srcMap = ParseUtils.asMap(src, path)
    val memberNameMap = mutable.Map[String, Long]()
    EnumSpec(path, SortedMap.from(
      srcMap.map { case (id, desc) =>
        val idLong = ParseUtils.asLong(id, path)
        val value = EnumValueSpec.fromYaml(desc, path ++ List(idLong.toString))

        memberNameMap.get(value.name).foreach { (prevIdLong) =>
          throw KSYParseError.withText(
            s"duplicate enum member ID: '${value.name}', previously defined at /${(path ++ List(prevIdLong.toString)).mkString("/")}",
            path ++ List(idLong.toString)
          )
        }
        memberNameMap.put(value.name, idLong)
        idLong -> value
      }
    ))
  }
}
