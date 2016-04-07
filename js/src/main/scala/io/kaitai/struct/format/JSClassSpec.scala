package io.kaitai.struct.format

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

@ScalaJSDefined
trait JSClassSpec extends js.Object {
  val meta: js.UndefOr[JSMetaSpec]
  val seq: js.UndefOr[js.Array[JSAttrSpec]]
  val types: js.UndefOr[js.Dictionary[JSClassSpec]]
  val instances: js.UndefOr[js.Dictionary[JSInstanceSpec]]
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
        Map() // TODO
      )
  }
}
