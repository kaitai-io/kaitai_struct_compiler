package io.kaitai.struct.format

object ParseUtils {
  def ensureLegalKeys(src: Map[String, Any], legalKeys: Set[String], path: List[String]) = {
    src.keys.foreach((key) =>
      if (!legalKeys.contains(key))
        throw new YAMLParseException(s"unknown key found, expected: ${legalKeys.toList.sorted.mkString(", ")}", path ++ List(key))
    )
  }

  // Unfortunately, these can't be parameterized with [T] - for some reason, `case Some(value: T)`
  // doesn't seem to work properly, checking that value is indeed of type T. Besides, there are some
  // variations when we need to do implicit type conversions.

  def getValueStr(src: Map[String, Any], field: String, path: List[String]): String = {
    src.get(field) match {
      case Some(value: String) =>
        value
      case unknown =>
        throw new YAMLParseException(s"expected string, got $unknown", path ++ List(field))
    }
  }

  def getOptValueStr(src: Map[String, Any], field: String, path: List[String]): Option[String] = {
    src.get(field) match {
      case None =>
        None
      case Some(value: String) =>
        Some(value)
      case Some(value: Int) =>
        Some(value.toString)
      case unknown =>
        throw new YAMLParseException(s"expected string, got $unknown", path ++ List(field))
    }
  }

  def getOptValueBool(src: Map[String, Any], field: String, path: List[String]): Option[Boolean] = {
    src.get(field) match {
      case None =>
        None
      case Some(value: Boolean) =>
        Some(value)
      case unknown =>
        throw new YAMLParseException(s"expected int, got $unknown", path ++ List(field))
    }
  }

  def getOptValueInt(src: Map[String, Any], field: String, path: List[String]): Option[Int] = {
    src.get(field) match {
      case None =>
        None
      case Some(value: Int) =>
        Some(value)
      case unknown =>
        throw new YAMLParseException(s"expected int, got $unknown", path ++ List(field))
    }
  }

  def asStr(src: Any, path: List[String]): String = {
    src match {
      case str: String =>
        str
      case n: Int =>
        n.toString
      case unknown =>
        throw new YAMLParseException(s"expected string, got $unknown", path)
    }
  }

  def asLong(src: Any, path: List[String]): Long = {
    src match {
      case n: Long =>
        n
      case n: Int =>
        n
      case unknown =>
        throw new YAMLParseException(s"expected int, got $unknown", path)
    }
  }

  def asMap(src: Any, path: List[String]): Map[Any, Any] = {
    src match {
      case srcMap: Map[Any, Any] =>
        srcMap
      case unknown =>
        throw new YAMLParseException(s"expected map, got $unknown", path)
    }
  }

  def asMapStr(src: Any, path: List[String]): Map[String, Any] = {
    src match {
      case anyMap: Map[Any, Any] =>
        anyMapToStrMap(anyMap, path)
      case unknown =>
        throw new YAMLParseException(s"expected map, got $unknown", path)
    }
  }

  def asMapStrStr(src: Any, path: List[String]): Map[String, String] = {
    src match {
      case anyMap: Map[Any, Any] =>
        anyMapToStrStrMap(anyMap, path)
      case unknown =>
        throw new YAMLParseException(s"expected map, got $unknown", path)
    }
  }

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
