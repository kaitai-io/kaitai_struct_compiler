package io.kaitai.struct.format

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.problems.KSYParseError

object ParseUtils {
  def ensureLegalKeys(src: Map[String, Any], legalKeys: Set[String], path: List[String], where: Option[String] = None) = {
    src.keys.foreach((key) =>
      if (!key.startsWith("-") && !legalKeys.contains(key)) {
        val msg = where match {
          case Some(ctx) => s"invalid key found in $ctx, allowed"
          case None => "unknown key found, expected"
        }
        throw KSYParseError.withText(s"$msg: ${legalKeys.toList.sorted.mkString(", ")}", path ++ List(key))
      }
    )
  }

  // Unfortunately, these can't be parameterized with [T] - for some reason, `case Some(value: T)`
  // doesn't seem to work properly, checking that value is indeed of type T. Besides, there are some
  // variations when we need to do implicit type conversions.

  def getValueStr(src: Map[String, Any], field: String, path: List[String]): String = {
    src.get(field) match {
      case Some(value) =>
        asStr(value, path ++ List(field))
      case None =>
        throw KSYParseError.noKey(field, path)
    }
  }

  def getValueMapStrStr(src: Map[String, Any], field: String, path: List[String]): Map[String, String] = {
    src.get(field) match {
      case Some(value) =>
        asMapStrStr(value, path ++ List(field))
      case None =>
        throw KSYParseError.noKey(field, path)
    }
  }

  def getOptValueStr(src: Map[String, Any], field: String, path: List[String]): Option[String] = {
    src.get(field) match {
      case None =>
        None
      case Some(value) =>
        Some(asStr(value, path ++ List(field)))
      case unknown =>
        throw KSYParseError.badType("string", unknown, path ++ List(field))
    }
  }

  def getOptValueBool(src: Map[String, Any], field: String, path: List[String]): Option[Boolean] = {
    src.get(field) match {
      case None =>
        None
      case Some(value: Boolean) =>
        Some(value)
      case unknown =>
        throw KSYParseError.badType("boolean", unknown, path ++ List(field))
    }
  }

  def getOptValueInt(src: Map[String, Any], field: String, path: List[String]): Option[Int] = {
    src.get(field) match {
      case None =>
        None
      case Some(value: Int) =>
        Some(value)
      case unknown =>
        throw KSYParseError.badType("int", unknown, path ++ List(field))
    }
  }

  def getOptValueByte(src: Map[String, Any], field: String, path: List[String]): Option[Int] = {
    getOptValueInt(src, field, path).map { value =>
      if (value < 0 || value > 255) {
        throw KSYParseError.withText(s"expected an integer from 0 to 255, got ${value}", path ++ List(field))
      }
      value
    }
  }

  def getValueIdentifier(src: Map[String, Any], idx: Int, entityName: String, path: List[String]): Identifier = {
    getOptValueStr(src, "id", path) match {
      case Some(idStr) =>
        try {
          NamedIdentifier(idStr)
        } catch {
          case _: InvalidIdentifier =>
            throw KSYParseError.invalidId(idStr, entityName, path ++ List("id"))
        }
      case None => NumberedIdentifier(idx)
    }
  }

  def getValueExpression(src: Map[String, Any], field: String, path: List[String]): Ast.expr = {
    try {
      Expressions.parse(getValueStr(src, field, path))
    } catch {
      case epe: Expressions.ParseException =>
        throw KSYParseError.expression(epe, path ++ List(field))
    }
  }

  def getOptValueExpression(src: Map[String, Any], field: String, path: List[String]): Option[Ast.expr] = {
    try {
      getOptValueStr(src, field, path).map(Expressions.parse)
    } catch {
      case epe: Expressions.ParseException =>
        throw KSYParseError.expression(epe, path ++ List(field))
    }
  }

  /**
    * Gets a list of T-typed values from a given YAML map's key "field",
    * reporting errors accurately and ensuring type safety.
    *
    * Ensures that this is indeed a valid list, and each list's element
    * is converted using `convertFunc` function. Lack of "field" key
    * results in an empty list.
    *
    * @param src YAML map to get list from
    * @param field key name in YAML map, value expected to be a list
    * @param convertFunc function that gets element of Any type, expected
    *                    to check its type and do the conversion
    * @param path path used to report YAML errors
    * @tparam T type of list's elements
    * @return
    */
  def getList[T](
    src: Map[String, Any],
    field: String,
    convertFunc: ((Any, List[String]) => (T)),
    path: List[String]
  ): List[T] = {
    val pathField = path ++ List(field)
    src.get(field) match {
      case Some(srcList: List[Any]) =>
        srcList.zipWithIndex.map { case (element, idx) =>
          convertFunc(element, pathField ++ List(idx.toString))
        }
      case Some(singleObject: T) =>
        List(singleObject)
      case None =>
        List()
      case unknown =>
        throw KSYParseError.badType("array", unknown, pathField)
    }
  }

  /**
    * Gets a list of strings from a given YAML map's key "field",
    * reporting errors accurately and ensuring type safety.
    * @param src YAML map to get list from
    * @param field key name in YAML map, value expected to be a list
    * @param path path used to report YAML errors
    * @return list of strings from YAML map
    */
  def getListStr(src: Map[String, Any], field: String, path: List[String]): List[String] =
    getList[String](src, field, asStr, path)

  def asStr(src: Any, path: List[String]): String = {
    src match {
      case str: String =>
        str
      case n: Int =>
        n.toString
      case n: Long =>
        n.toString
      case n: Double =>
        n.toString
      case n: Boolean =>
        n.toString
      case unknown =>
        throw KSYParseError.badType("string", unknown, path)
    }
  }

  def asLong(src: Any, path: List[String]): Long = {
    src match {
      case n: Long =>
        n
      case n: Int =>
        n
      case str: String =>
        // Generally should not happen, but when the data comes from JavaScript,
        // all object keys are forced to be strings.
        try {
          Utils.strToLong(str)
        } catch {
          case ex: MatchError =>
            throw KSYParseError.withText(s"unable to parse `$str` as int", path)
        }
      case unknown =>
        throw KSYParseError.badType("int", unknown, path)
    }
  }

  def asExpression(src: Any, path: List[String]): Ast.expr = {
    try {
      Expressions.parse(asStr(src, path))
    } catch {
      case epe: Expressions.ParseException =>
        throw KSYParseError.expression(epe, path)
    }
  }

  def asMap(src: Any, path: List[String]): Map[Any, Any] = {
    src match {
      case srcMap: Map[Any, Any] =>
        srcMap
      case unknown =>
        throw KSYParseError.badType("map", unknown, path)
    }
  }

  def asMapStr(src: Any, path: List[String]): Map[String, Any] =
    anyMapToStrMap(asMap(src, path), path)

  def asMapStrStr(src: Any, path: List[String]): Map[String, String] =
    anyMapToStrStrMap(asMap(src, path), path)

  def anyMapToStrMap(anyMap: Map[Any, Any], path: List[String]): Map[String, Any] = {
    anyMap.map { case (key, value) =>
      val keyStr = asStr(key, path)
      keyStr -> value
    }
  }

  def anyMapToStrStrMap(anyMap: Map[Any, Any], path: List[String]): Map[String, String] = {
    anyMap.map { case (key, value) =>
      val keyStr = asStr(key, path)
      val valueStr = asStr(value, path ++ List(keyStr))
      keyStr -> valueStr
    }
  }
}
