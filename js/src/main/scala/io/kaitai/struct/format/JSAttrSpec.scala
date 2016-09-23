package io.kaitai.struct.format

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

@ScalaJSDefined
trait JSAttrSpec extends js.Object {
  val id: js.UndefOr[String]
  val `type`: js.UndefOr[String]
  val process: js.UndefOr[String]
  val contents: js.UndefOr[String]
  val size: js.UndefOr[js.Object]
  val `size-eos`: js.UndefOr[Boolean]
  val `if`: js.UndefOr[String]
  val encoding: js.UndefOr[String]
  val repeat: js.UndefOr[String]
  val `repeat-expr`: js.UndefOr[js.Object]
  val `repeat-until`: js.UndefOr[js.Object]
  val terminator: js.UndefOr[js.Object]
  val consume: js.UndefOr[String]
  val include: js.UndefOr[String]
  val `eos-error`: js.UndefOr[String]
  val enum: js.UndefOr[String]
}

object JSAttrSpec {
  implicit class JSAttrSpecOps(val self: JSAttrSpec) extends AnyVal {
    def toScala: AttrSpec =
      AttrSpec.create(
        self.id.orNull,
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
        self.enum.orNull
      )
  }
}
