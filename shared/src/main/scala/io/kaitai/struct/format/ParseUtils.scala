package io.kaitai.struct.format

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.problems.KSYParseError
import yamlesque.Obj

import scala.collection.mutable

object ParseUtils {
  def ensureLegalKeys(src: yamlesque.Obj, legalKeys: Set[String], path: List[String], where: Option[String] = None) = {
    src.obj.keys.foreach((key) =>
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

  def getValueStr(src: yamlesque.Obj, field: String, path: List[String]): String = {
    src.obj.get(field) match {
      case Some(value) =>
        asStr(value, path ++ List(field))
      case None =>
        throw KSYParseError.noKey(path ++ List(field))
    }
  }

  def getValueMapStrStr(src: yamlesque.Obj, field: String, path: List[String]): Map[String, String] = {
    src.obj.get(field) match {
      case Some(value) =>
        asMapStrStr(value, path ++ List(field))
      case None =>
        throw KSYParseError.noKey(path ++ List(field))
    }
  }

  def getOptValueStr(src: yamlesque.Obj, field: String, path: List[String]): Option[String] = {
    src.obj.get(field) match {
      case None =>
        None
      case Some(value) =>
        Some(asStr(value, path ++ List(field)))
      case unknown =>
        throw KSYParseError.badType("string", unknown, path ++ List(field))
    }
  }

  def getOptValueBool(src: yamlesque.Obj, field: String, path: List[String]): Option[Boolean] = {
    src.obj.get(field) match {
      case None =>
        None
      case Some(yamlesque.Bool(value)) =>
        Some(value)
      case unknown =>
        throw KSYParseError.badType("boolean", unknown, path ++ List(field))
    }
  }

  def getOptValueInt(src: yamlesque.Obj, field: String, path: List[String]): Option[Int] = {
    src.obj.get(field) match {
      case None =>
        None
      case Some(yamlesque.Num(value)) =>
        Some(value.toInt) // FIXME: what to do with actual doubles?
      case unknown =>
        throw KSYParseError.badType("int", unknown, path ++ List(field))
    }
  }

  def getValueIdentifier(src: yamlesque.Obj, idx: Int, entityName: String, path: List[String]): Identifier = {
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

  def getValueExpression(src: yamlesque.Obj, field: String, path: List[String]): Ast.expr = {
    try {
      Expressions.parse(getValueStr(src, field, path))
    } catch {
      case epe: Expressions.ParseException =>
        throw KSYParseError.expression(epe, path)
    }
  }

  def getOptValueExpression(src: yamlesque.Obj, field: String, path: List[String]): Option[Ast.expr] = {
    try {
      getOptValueStr(src, field, path).map(Expressions.parse)
    } catch {
      case epe: Expressions.ParseException =>
        throw KSYParseError.expression(epe, path)
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
    * @param src YAML map to get list from
    * @param field key name in YAML map, value expected to be a list
    * @param convertFunc function that gets element of Any type, expected
    *                    to check its type and do the conversion
    * @param path path used to report YAML errors
    * @tparam T type of list's elements
    * @return
    */
  def getList[T](
    src: yamlesque.Obj,
    field: String,
    convertFunc: ((Any, List[String]) => (T)),
    path: List[String]
  ): List[T] = {
    val pathField = path ++ List(field)
    src.obj.get(field) match {
      case Some(yamlesque.Arr(srcList)) =>
        srcList.zipWithIndex.map { case (element, idx) =>
          convertFunc(element, pathField ++ List(idx.toString))
        }.toList
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
    * @param src YAML map to get list from
    * @param field key name in YAML map, value expected to be a list
    * @param path path used to report YAML errors
    * @return list of strings from YAML map
    */
  def getListStr(src: yamlesque.Obj, field: String, path: List[String]): List[String] =
    getList[String](src, field, asStr, path)

  def asStr(src: Any, path: List[String]): String = {
    src match {
      case s: yamlesque.Str =>
        s.str
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

  def asMap(src: yamlesque.Node, path: List[String]): yamlesque.Obj = {
    src match {
      case obj: yamlesque.Obj =>
        obj
      case unknown =>
        throw KSYParseError.badType("map", unknown, path)
    }
  }

  def asMapStr(src: yamlesque.Node, path: List[String]): yamlesque.Obj = src match {
    case obj: Obj => obj
    case unknown =>
      throw KSYParseError.badType("map", unknown, path)
  }

  def asMapStrStr(src: yamlesque.Node, path: List[String]): Map[String, String] =
    anyMapToStrStrMap(asMap(src, path), path)

  def anyMapToStrMap(anyMap: yamlesque.Obj, path: List[String]): Map[String, Any] = {
    anyMap.obj.map { case (key, value) =>
      val keyStr = asStr(key, path)
      keyStr -> value
    }.toMap
  }

  def anyMapToStrStrMap(anyMap: yamlesque.Node, path: List[String]): Map[String, String] = {
    anyMap.obj.map { case (key, value) =>
      val valueStr = asStr(value, path ++ List(key))
      key -> valueStr
    }.toMap
  }

  def mapToYamlObj(m: scala.collection.Map[String, yamlesque.Node]): yamlesque.Obj = {
    val res = new mutable.LinkedHashMap[String, yamlesque.Node]()
    m.foreach { case (k, v) => res(k) = v }
    yamlesque.Obj(res)
  }
}
