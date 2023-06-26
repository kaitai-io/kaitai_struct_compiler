package io.kaitai.struct.format

import io.kaitai.struct.problems.KSYParseError

import scala.collection.mutable

case class EnumSpec(path: List[String], map: Map[Long, EnumValueSpec]) extends YAMLPath {
  var name = List[String]()

  /**
    * @return Absolute name of enum as string, components separated by
    *         double colon operator `::`
    */
  def nameAsStr = name.mkString("::")

  /**
    * Stabilize order of generated enums by sorting it by integer ID - it
    * both looks nicer and doesn't screw diffs in generated code.
    */
  lazy val sortedSeq: Seq[(Long, EnumValueSpec)] = map.toSeq.sortBy(_._1)
}

object EnumSpec {
  def fromYaml(src: Any, path: List[String]): EnumSpec = {
    val srcMap = ParseUtils.asMap(src, path)
    val memberNameMap = mutable.Map[String, Long]()
    EnumSpec(path, srcMap.map { case (id, desc) =>
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
    })
  }
}
