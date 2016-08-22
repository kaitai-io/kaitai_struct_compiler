package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format.{InstanceIdentifier, _}
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}
import io.kaitai.struct.translators.{BaseTranslator, RubyTranslator, TypeProvider}
import io.kaitai.struct.{LanguageOutputWriter, Utils}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GraphvizCompiler(verbose: Boolean, out: LanguageOutputWriter) extends LanguageCompiler(verbose, out) {
  import GraphvizCompiler._

  var seqPos: Option[Int] = None
  var curClass = mutable.Stack[List[String]]()
  var lastInstPos: Option[Ast.expr] = None
  var links = ListBuffer[(String, String)]()

  override def getStatic = GraphvizCompiler

  override def fileHeader(topClassName: String): Unit = {
    out.puts("digraph {")
    out.inc
    out.puts("rankdir=LR;")
    out.puts("node [shape=plaintext];")
  }

  override def fileFooter(topClassName: String): Unit = {
    links.foreach { case (p1, p2) =>
      out.puts(s"$p1 -> $p2;")
    }

    out.dec
    out.puts("}")
  }

  override def classHeader(name: List[String]): Unit = {
    out.puts(s"subgraph cluster__${type2class(name)} {")
    out.inc
    out.puts("label=\"" + type2display(name) + "\";")
    out.puts("graph[style=dotted];")
    out.puts

    curClass.push(name)
  }

  override def classFooter(name: List[String]): Unit = {
    out.dec
    out.puts("}")

    curClass.pop
  }

  val HEADER_BGCOLOR = "#E0FFE0"
  val TH_START = "<TD BGCOLOR=\"" + HEADER_BGCOLOR + "\">"

  override def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit = {
    out.puts(s"${type2class(name)}__seq" + " [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">")
    out.inc
    out.puts(s"<TR>${TH_START}pos</TD>${TH_START}size</TD>${TH_START}type</TD>${TH_START}id</TD></TR>")

    seqPos = Some(0)
  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("</TABLE>>];")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: BaseType): Unit = {}

  override def attrParse(attr: AttrLikeSpec, id: Identifier, extraAttrs: ListBuffer[AttrSpec], io: String): Unit = {
    id match {
      case NamedIdentifier(name) =>
        doTableRow(seqPos.map(_.toString), attr.dataType, name, "seq")

        val size = dataTypeSize(attr.dataType)
        seqPos = (seqPos, size) match {
          case (Some(pos), Some(siz)) => Some(pos + siz)
          case _ => None
        }
      case InstanceIdentifier(name) =>
        lastInstPos match {
          case Some(pos) =>
            val (posStr, posAffected) = expression(pos)
            doTableRow(Some(posStr), attr.dataType, name, s"inst__$name")

            // Add pos links
            posAffected.foreach((tgt) =>
              links += ((resolveNode(tgt), s"${type2class(curClass.head)}__inst__$name:${name}_pos"))
            )

          case None =>
            doTableRow(None, attr.dataType, name, s"inst__$name")
        }
      case _ =>
        // ignore
    }
  }

  def doTableRow(pos: Option[String], dataType: BaseType, name: String, nodeAdd: String): Unit = {
    val (sizeStr, sizeAffected) = dataTypeSizeAsString(dataType)

    out.puts("<TR>" +
      "<TD PORT=\"" + name + "_pos\">" + pos.getOrElse("...") + "</TD>" +
      "<TD PORT=\"" + name + "_size\">" + sizeStr + "</TD>" +
      s"<TD>${dataTypeName(dataType)}</TD>" +
      "<TD PORT=\"" + name + "_type\">" + name + "</TD>" +
      "</TR>")

    // Add size links
    sizeAffected.foreach((tgt) =>
      links += ((resolveNode(tgt), s"${type2class(curClass.head)}__$nodeAdd:${name}_size"))
    )

    // Add user type links
    dataType match {
      case ut: UserType =>
        links += ((s"${type2class(curClass.head)}__$nodeAdd:${name}_type", type2class(ut.name) + "__seq"))
      case _ =>
      // ignore, no links
    }
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = {}

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {}

  override def normalIO: String = ""

  override def useIO(ioEx: Ast.expr): String = ""

  override def pushPos(io: String): Unit = {}

  override def seek(io: String, pos: Ast.expr): Unit = {
    lastInstPos = Some(pos)
  }

  override def popPos(io: String): Unit = {}

  override def condIfHeader(expr: Ast.expr): Unit = {}

  override def condIfFooter(expr: Ast.expr): Unit = {}

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {}

  override def condRepeatEosFooter: Unit = {}

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    id match {
      case NamedIdentifier(name) => condRepeatExprHeader(name, repeatExpr)
      case InstanceIdentifier(name) => condRepeatExprHeader(name, repeatExpr)
    }
  }

  def condRepeatExprHeader(name: String, repeatExpr: Ast.expr): Unit = {
    val (repeatExprStr, affected) = expression(repeatExpr)
    val portName = s"${name}_repeat_expr"

    out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat " + repeatExprStr + " times</TD></TR>")

    affected.foreach((s) => links += ((portName, resolveNode(s))))
  }

  override def condRepeatExprFooter: Unit = {}

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {}

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {}

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: BaseType): Unit = {
    out.puts(s"${type2class(className)}__inst__${instName.name}" + " [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">")
    out.inc
    out.puts(s"<TR>${TH_START}pos</TD>${TH_START}size</TD>${TH_START}type</TD>${TH_START}id</TD></TR>")

    lastInstPos = None
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {}

  override def instanceReturn(instName: InstanceIdentifier): Unit = {}

  override def instanceCalculate(instName: InstanceIdentifier, dataType: BaseType, value: Ast.expr): Unit = {}

  override def instanceFooter: Unit = {
    out.dec
    out.puts("</TABLE>>];")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Map[Long, String]): Unit = {}

  def expression(e: expr): (String, List[String]) = {
    (translator.translate(e), affectedVars(e))
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

  val END_OF_STREAM = "⇲"
  val END_OF_STREAM_TUPLE = (END_OF_STREAM, List[String]())
  val UNKNOWN = "..."
  val UNKNOWN_TUPLE = (UNKNOWN, List[String]())

  /**
    * Determine visual interpretation of data type's size to be used in
    * a displayed table.
    *
    * @param dataType data type to analyze
    * @return a tuple; first element is a String that should be displayed in
    *         table's column "size"; second element is a list of all the
    *         affected variables used in this expression to add links to
    */
  def dataTypeSizeAsString(dataType: BaseType): (String, List[String]) = {
    dataType match {
      case _: Int1Type => ("1", List())
      case IntMultiType(_, width, _) => (width.width.toString, List())
      case FixedBytesType(contents, _) => (contents.length.toString, List())
      case BytesEosType(_) => END_OF_STREAM_TUPLE
      case BytesLimitType(ex, _) => expression(ex)
      case StrByteLimitType(ex, _) => expression(ex)
      case StrEosType(_) => END_OF_STREAM_TUPLE
      case _: StrZType => UNKNOWN_TUPLE
      case UserTypeByteLimit(_, ex, _) => expression(ex)
      case _: UserTypeEos => END_OF_STREAM_TUPLE
      case UserTypeInstream(_) => UNKNOWN_TUPLE
      case EnumType(_, basedOn) => dataTypeSizeAsString(basedOn)
    }
  }

  def resolveNode(s: String): String = {
    s"${type2class(curClass.head)}__seq:${s}_type"
  }
}

object GraphvizCompiler extends LanguageCompilerStatic {
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
