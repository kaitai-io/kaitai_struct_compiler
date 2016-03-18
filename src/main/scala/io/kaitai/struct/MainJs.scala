package io.kaitai.struct

import io.kaitai.struct.format.{AttrSpec, ClassSpec, MetaSpec}
import io.kaitai.struct.languages._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, ScalaJSDefined}

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
        self.terminator.map(x => x.toString).orNull,
        self.consume.orNull,
        self.include.orNull,
        self.`eos-error`.orNull,
        self.enum.orNull
      )
  }
}

@ScalaJSDefined
trait JSClassSpec extends js.Object {
  val meta: js.UndefOr[JSMetaSpec]
  val seq: js.UndefOr[js.Array[JSAttrSpec]]
  val types: js.UndefOr[js.Dictionary[JSClassSpec]]
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
        Map(), // TODO
        Map() // TODO
      )
  }
}

@JSExport
object MainJs {
  @JSExport
  def compile(lang: String, yaml: JSClassSpec): String = {
    val config = new Main.Config(verbose = true)

    val (out, cc) = ClassCompiler.fromClassSpecToString(yaml.toScala, LanguageCompilerStatic.byString(lang), config)
    cc.compile
    out.result
  }
}
