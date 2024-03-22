package io.kaitai.struct

import io.kaitai.struct.translators.CommonLiterals
import scala.collection

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
      case v: collection.Seq[_] => seqToJson(v)
      case v: Map[String, _] => mapToJson(v)
    }
  }

  /** octal escapes (which [[translators.CommonLiterals.strLiteralGenericCC]] uses by default) are not allowed in JSON */
  override def strLiteralGenericCC(code: Char): String = strLiteralUnicode(code)

  def stringToJson(str: String): String =
    doStringLiteral(str)

  def seqToJson(obj: collection.Seq[_]): String =
    "[" + obj.map((x) => stringify(x)).mkString(",") + "]"

  def mapToJson(obj: Map[String, Any]): String = {
    val kvs = obj.map { case (k, v) =>
      stringToJson(k) + ": " + stringify(v)
    }
    "{" + kvs.mkString(",") + "}"
  }
}
