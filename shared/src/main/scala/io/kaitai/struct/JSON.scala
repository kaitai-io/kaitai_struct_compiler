package io.kaitai.struct

import io.kaitai.struct.translators.CommonLiterals

/** Common trait for all objects that can be serialized as JSON. */
trait Jsonable {
  /** Serialize current state of the object into JSON string. */
  def toJson: String
}

/**
  * Ultra-minimalistic JSON strings generator from arbitrary Scala objects.
  */
object JSON extends CommonLiterals {
  /**
    * Converts an arbitrary Scala object to JSON string representation.
    * @param obj object to convert
    * @return string representation in JSON format
    */
  def stringify(obj: Any): String = {
    obj match {
      case v: Jsonable => v.toJson
      case v: Int => v.toString
      case v: String => stringToJson(v)
      case v: List[_] => listToJson(v)
      case v: Map[String, _] => mapToJson(v)
    }
  }

  def stringToJson(str: String): String =
    doStringLiteral(str)

  def listToJson(obj: List[_]): String =
    "[" + obj.map((x) => stringify(x)).mkString(",") + "]"

  def mapToJson(obj: Map[String, _]): String = {
    val kvs = obj.map { case (k, v) =>
      stringToJson(k) + ": " + stringify(v)
    }
    "{" + kvs.mkString(",") + "}"
  }
}
