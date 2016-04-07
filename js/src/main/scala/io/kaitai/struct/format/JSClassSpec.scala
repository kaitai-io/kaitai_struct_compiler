package io.kaitai.struct.format

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

@ScalaJSDefined
trait JSClassSpec extends js.Object {
  val meta: js.UndefOr[JSMetaSpec]
  val seq: js.UndefOr[js.Array[JSAttrSpec]]
  val types: js.UndefOr[js.Dictionary[JSClassSpec]]
  val instances: js.UndefOr[js.Dictionary[JSInstanceSpec]]
  val enums: js.UndefOr[js.Dictionary[js.Dictionary[String]]]
}

object JSClassSpec {
  implicit class JSClassSpecOps(val self: JSClassSpec) extends AnyVal {
    def toScala: ClassSpec =
      ClassSpec(
        self.meta.toOption.map(x => x.toScala),
        self.seq.toOption match {
          case Some(x) => x.map(x => x.toScala).toList
          case None => List()
        },
        self.types.toOption match {
          case Some(x) =>
            x.map { case (key, value) =>
              key -> value.toScala
            }.toMap
          case None => Map()
        },
        self.instances.toOption match {
          case Some(x) =>
            x.map { case (key, value) =>
              key -> value.toScala
            }.toMap
          case None => Map()
        },
        self.enums.toOption match {
          case Some(x) =>
            x.map { case (enumName, enumDict) =>
              enumName -> enumDict.map {
                case (enumKey, enumValue) =>
                  enumKey.toLong -> enumValue
              }.toMap
            }.toMap
          case None => Map()
        }
      )
  }
}
