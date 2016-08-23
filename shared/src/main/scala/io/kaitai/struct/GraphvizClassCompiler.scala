package io.kaitai.struct

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.LanguageCompilerStatic
import io.kaitai.struct.translators.{BaseTranslator, RubyTranslator, TypeProvider}

import scala.collection.mutable.ListBuffer

class GraphvizClassCompiler(topClass: ClassSpec, out: LanguageOutputWriter) extends AbstractCompiler {
  import GraphvizClassCompiler._

  val translator = getTranslator(this)
  val links = ListBuffer[(String, String)]()

  var currentTable: String = ""

  override def compile: Unit = {
    out.puts("digraph {")
    out.inc
    out.puts("rankdir=LR;")
    out.puts("node [shape=plaintext];")

    compileClass(List(topClass.meta.get.id), topClass)

    links.foreach { case (t1, t2) =>
        out.puts(s"$t1 -> $t2;")
    }

    out.dec
    out.puts("}")
  }

  def compileClass(className: List[String], curClass: ClassSpec): Unit = {
    out.puts(s"subgraph cluster__${type2class(className)} {")
    out.inc
    out.puts("label=\"" + type2display(className) + "\";")
    out.puts("graph[style=dotted];")
    out.puts

    // Sequence
    compileSeq(className, curClass)

    curClass.instances.foreach { case (instName, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec =>
          tableStart(className, s"inst__${instName.name}")
          compileInstance(className, instName, pis)
          tableEnd
        case _: ValueInstanceSpec =>
          // ignore for now
      }
    }

//    curClass.enums.foreach { case(enumName, enumColl) => compileEnum(enumName, enumColl) }

    // Recursive types
    curClass.types.foreach { case (typeName, intClass) => compileClass(className :+ typeName, intClass) }

    out.dec
    out.puts("}")
  }

  def compileSeq(className: List[String], curClass: ClassSpec): Unit = {
    tableStart(className, "seq")
    var seqPos: Option[Int] = Some(0)
    curClass.seq.foreach { (attr) =>
      attr.id match {
        case NamedIdentifier(name) =>
          tableRow(className, seqPos.map(_.toString), attr.dataType, name)

          val tableNode = s"${type2class(className)}__seq"

          val portName = name + "__repeat"
          attr.cond.repeat match {
            case RepeatExpr(ex) =>
              out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat " +
                expression(ex, s"$tableNode:$portName") +
                " times</TD></TR>")
            case RepeatUntil(ex) =>
              out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat until " +
                expression(ex, s"$tableNode:$portName") +
                "</TD></TR>")
            case RepeatEos =>
              out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat to end of stream</TD></TR>")
            case NoRepeat =>
              // no additional line
          }

          val size = dataTypeSize(attr.dataType)
          seqPos = (seqPos, size) match {
            case (Some(pos), Some(siz)) => Some(pos + siz)
            case _ => None
          }
      }
    }
    tableEnd
  }

  def compileInstance(className: List[String], id: InstanceIdentifier, inst: ParseInstanceSpec): Unit = {
    val name = id.name
    val lastInstPos = inst.pos
    lastInstPos match {
      case Some(pos) =>
        val posStr = expression(pos, s"${name}_pos")
        tableRow(className, Some(posStr), inst.dataType, name)

      case None =>
        tableRow(className, None, inst.dataType, name)
    }
  }

  val HEADER_BGCOLOR = "#E0FFE0"
  val TH_START = "<TD BGCOLOR=\"" + HEADER_BGCOLOR + "\">"

  def tableStart(className: List[String], extra: String): Unit = {
    currentTable = s"${type2class(className)}__$extra"
    out.puts(s"$currentTable" + " [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">")
    out.inc
    out.puts(s"<TR>${TH_START}pos</TD>${TH_START}size</TD>${TH_START}type</TD>${TH_START}id</TD></TR>")
  }

  def tableEnd: Unit = {
    out.dec
    out.puts("</TABLE>>];")
  }

  def tableRow(curClass: List[String], pos: Option[String], dataType: BaseType, name: String): Unit = {
    val sizeStr = dataTypeSizeAsString(dataType, name)

    out.puts("<TR>" +
      "<TD PORT=\"" + name + "_pos\">" + pos.getOrElse("...") + "</TD>" +
      "<TD PORT=\"" + name + "_size\">" + sizeStr + "</TD>" +
      s"<TD>${dataTypeName(dataType)}</TD>" +
      "<TD PORT=\"" + name + "_type\">" + name + "</TD>" +
      "</TR>")

    // Add user type links
    dataType match {
      case ut: UserType =>
        links += ((s"$currentTable:${name}_type", type2class(ut.name) + "__seq"))
      case _ =>
      // ignore, no links
    }
  }

  val END_OF_STREAM = "⇲"
  val UNKNOWN = "..."

  /**
    * Determine visual interpretation of data type's size to be used in
    * a displayed table.
    *
    * @param dataType data type to analyze
    * @param attrName attribute name to be used to generate port name for affected vars linking
    * @return a String that should be displayed in table's column "size"
    */
  def dataTypeSizeAsString(dataType: BaseType, attrName: String): String = {
    dataType match {
      case _: Int1Type => "1"
      case IntMultiType(_, width, _) => width.width.toString
      case FixedBytesType(contents, _) => contents.length.toString
      case BytesEosType(_) => END_OF_STREAM
      case BytesLimitType(ex, _) => expression(ex, s"${attrName}_size")
      case StrByteLimitType(ex, _) => expression(ex, s"${attrName}_size")
      case StrEosType(_) => END_OF_STREAM
      case _: StrZType => UNKNOWN
      case UserTypeByteLimit(_, ex, _) => expression(ex, s"${attrName}_size")
      case _: UserTypeEos => END_OF_STREAM
      case UserTypeInstream(_) => UNKNOWN
      case EnumType(_, basedOn) => dataTypeSizeAsString(basedOn, attrName)
    }
  }

  def expression(e: expr, portName: String): String = {
    affectedVars(e).foreach((v) =>
      links += ((resolveNode(v), portName))
    )
    translator.translate(e)
  }

  def affectedVars(e: expr): List[String] = {
    e match {
      case expr.BoolOp(op, values) =>
        values.flatMap(affectedVars).toList
      case expr.BinOp(left, op, right) =>
        affectedVars(left) ++ affectedVars(right)
      case expr.UnaryOp(op, operand) =>
        affectedVars(operand)
      case expr.IfExp(condition, ifTrue, ifFalse) =>
        affectedVars(condition) ++ affectedVars(ifTrue) ++ affectedVars(ifFalse)
      //      case expr.Dict(keys, values) =>
      case expr.Compare(left, ops, right) =>
        affectedVars(left) ++ affectedVars(right)
      //      case expr.Call(func, args) =>
      case expr.IntNum(_) | expr.FloatNum(_) | expr.Str(_) =>
        List()
      //      case expr.EnumByLabel(enumName, label) =>
      //      case expr.Attribute(value, attr) =>
      case expr.Subscript(value, idx) =>
        affectedVars(value) ++ affectedVars(idx)
      case expr.Name(id) =>
        List(id.name)
      case expr.List(elts) =>
        elts.flatMap(affectedVars).toList
    }
  }

  def resolveNode(s: String): String = {
    s"$currentTable:${s}_type"
  }

  override def determineType(parentType: List[String], attrName: String): BaseType = ???

  override def determineType(attrName: String): BaseType = ???
}

object GraphvizClassCompiler extends LanguageCompilerStatic {
  override def indent: String = "\t"
  override def outFileName(topClassName: String): String = s"$topClassName.dot"
  override def getTranslator(tp: TypeProvider): BaseTranslator = new RubyTranslator(tp)

  def type2class(name: List[String]) = name.last
  def type2display(name: List[String]) = name.map(Utils.upperCamelCase).mkString("::")

  /**
    * Determines how many bytes occupies given data type.
    *
    * @param dataType data type to analyze
    * @return number of bytes or None, if it's impossible to determine a priori
    */
  def dataTypeSize(dataType: BaseType): Option[Int] = {
    dataType match {
      case _: Int1Type => Some(1)
      case IntMultiType(_, width, _) => Some(width.width)
      case FixedBytesType(contents, _) => Some(contents.length)
      case BytesEosType(_) => None
      case BytesLimitType(ex, _) => evaluateIntLiteral(ex)
      case StrByteLimitType(ex, _) => evaluateIntLiteral(ex)
      case StrEosType(_) => None
      case _: StrZType => None
      case UserTypeByteLimit(_, ex, _) => evaluateIntLiteral(ex)
      case _: UserTypeEos => None
      case UserTypeInstream(_) => None
      case EnumType(_, basedOn) => dataTypeSize(basedOn)
    }
  }

  def dataTypeName(dataType: BaseType): String = {
    dataType match {
      case rt: ReadableType => rt.apiCall
      case ut: UserType => type2display(ut.name)
      case FixedBytesType(contents, _) => contents.map(_.formatted("%02X")).mkString(" ")
      case _ => dataType.toString
    }
  }

  /**
    * Evaluates the expression, if possible to get the result without introduction
    * of any variables or anything.
    *
    * @param expr expression to evaluate
    * @return integer result or None
    */
  def evaluateIntLiteral(expr: Ast.expr): Option[Int] = {
    expr match {
      case Ast.expr.IntNum(x) => Some(x.toInt)
      case _ => None
    }
  }
}
