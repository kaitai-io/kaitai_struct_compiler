package io.kaitai.struct

import scala.collection.mutable.ListBuffer
import io.kaitai.struct.datatype.DataType.{CalcIntType, KaitaiStreamType, UserTypeInstream}
import io.kaitai.struct.datatype.{BigEndian, CalcEndian, Endianness, FixedEndian, InheritedEndian, LittleEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.Ast.identifier
import io.kaitai.struct.exprlang.Ast.expr.{BoolOp, BinOp, UnaryOp, IfExp, Compare, Call, Attribute, CastToType, Subscript, Name}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.CCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

class CClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, CCompiler) {

}
