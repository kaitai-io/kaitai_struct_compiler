package io.kaitai.struct.format

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import io.kaitai.struct.exprlang.DataType.BaseType
import io.kaitai.struct.exprlang.{Ast, Expressions}

// Note: can't be "sealed trait" due to Java JSON parser library compatibility!
abstract class InstanceSpec(val doc: Option[String])
case class ValueInstanceSpec(_doc: Option[String], value: Ast.expr, var dataType: Option[BaseType]) extends InstanceSpec(_doc)
case class ParseInstanceSpec(_doc: Option[String], dataType: BaseType, cond: ConditionalSpec, pos: Option[Ast.expr], io: Option[Ast.expr]) extends InstanceSpec(_doc) with AttrLikeSpec

object InstanceSpec {
  @JsonCreator
  def create(
              @JsonProperty("doc") doc: String,
              @JsonProperty("type") dataType: String,
              @JsonProperty("process") process: String,
              @JsonProperty("contents") contents: Object,
              @JsonProperty("size") _size: String,
              @JsonProperty("size-eos") sizeEos: Boolean,
              @JsonProperty("if") _ifExpr: String,
              @JsonProperty("encoding") _encoding: String,
              @JsonProperty("repeat") _repeat: String,
              @JsonProperty("repeat-expr") _repeatExpr: String,
              @JsonProperty("repeat-until") _repeatUntil: String,
              @JsonProperty("terminator") _terminator: String,
              @JsonProperty("consume") _consume: String,
              @JsonProperty("include") _include: String,
              @JsonProperty("eos-error") _eosError: String,
              @JsonProperty("enum") _enum: String,

              @JsonProperty("pos") _pos: String,
              @JsonProperty("io") _io: String,
              @JsonProperty("value") _value: String
            ): InstanceSpec = {
    val positionAbs = Option(_pos).map(Expressions.parse)
    val io = Option(_io).map(Expressions.parse)

    val value = Option(_value).map(e =>
      if (dataType != null) {
        throw new RuntimeException("instance: can't specify both 'value' and 'type'")
      } else if (process != null) {
        throw new RuntimeException("instance: can't specify both 'value' and 'process'")
      } else if (contents != null) {
        throw new RuntimeException("instance: can't specify both 'value' and 'contents'")
      } else if (_size != null) {
        throw new RuntimeException("instance: can't specify both 'value' and 'size'")
      } else if (_repeat != null) {
        throw new RuntimeException("instance: can't specify both 'value' and 'repeat'")
      } else if (_repeatExpr != null) {
        throw new RuntimeException("instance: can't specify both 'value' and 'repeat-expr'")
      } else if (positionAbs.isDefined) {
        throw new RuntimeException("instance: can't specify both 'value' and 'pos'")
      } else {
        Expressions.parse(e)
      }
    )

    value match {
/*      case None =>
        val a = AttrSpec.create(
          "fake_id",
          doc,
          dataType,
          process,
          contents,
          _size,
          sizeEos,
          _ifExpr,
          _encoding,
          _repeat,
          _repeatExpr,
          _repeatUntil,
          _terminator,
          _consume,
          _include,
          _eosError,
          _enum
        )
        ParseInstanceSpec(a.doc, a.dataType, a.cond, positionAbs, io)*/
      case Some(v) =>
        ValueInstanceSpec(Option(doc), v, None)
    }
  }
}
