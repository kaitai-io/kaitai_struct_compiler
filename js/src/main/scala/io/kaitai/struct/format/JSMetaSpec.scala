package io.kaitai.struct.format

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

@ScalaJSDefined
trait JSMetaSpec extends js.Object {
  val id: String
  val endian: js.UndefOr[String]
}

object JSMetaSpec {
  implicit class JSMetaSpecOps(val self: JSMetaSpec) extends AnyVal {
    def toScala: MetaSpec =
      MetaSpec.create(
        self.id,
        self.endian.orNull,
        null,
        null
      )
  }
}
