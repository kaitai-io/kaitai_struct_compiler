package io.kaitai.struct.format

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

@ScalaJSDefined
trait JSInstanceSpec extends JSAttrSpec {
  val pos: js.UndefOr[String]
  val io: js.UndefOr[String]
  val value: js.UndefOr[String]
}

object JSInstanceSpec {
  implicit class JSInstanceSpecOps(val self: JSInstanceSpec) extends AnyVal {
    def toScala: InstanceSpec =
      InstanceSpec.create(
        self.`type`.orNull,
        self.process.orNull,
        self.contents.orNull,
        self.size.map(x => x.toString).orNull,
        self.`size-eos`.getOrElse(false),
        self.`if`.orNull,
        self.encoding.orNull,
        self.repeat.orNull,
        self.`repeat-expr`.map(x => x.toString).orNull,
        self.`repeat-until`.map(x => x.toString).orNull,
        self.terminator.map(x => x.toString).orNull,
        self.consume.orNull,
        self.include.orNull,
        self.`eos-error`.orNull,
        self.enum.orNull,

        self.pos.orNull,
        self.io.orNull,
        self.value.orNull
      )
  }
}
