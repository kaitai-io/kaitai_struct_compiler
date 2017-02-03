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

  val topClassName = List(topClass.meta.get.id)

  val provider = new ClassTypeProvider(topClass)
  val translator = getTranslator(provider)
  val links = ListBuffer[(String, String, String)]()
  val extraClusterLines = new StringLanguageOutputWriter(GraphvizClassCompiler.indent)

  def nowClass: ClassSpec = provider.nowClass
  def nowClassName = provider.nowClass.name
  var currentTable: String = ""

  override def compile: Unit = {
    out.puts("digraph {")
    out.inc
    out.puts("rankdir=LR;")
    out.puts("node [shape=plaintext];")

    compileClass(topClass)

    links.foreach { case (t1, t2, style) =>
        out.puts(s"$t1 -> $t2 [$style];")
    }

    out.dec
    out.puts("}")
  }

  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass
    val className = curClass.name

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
          compileParseInstance(className, instName, pis)
          tableEnd
        case vis: ValueInstanceSpec =>
          tableValueInstance(className, instName.name, vis)
      }
    }

//    curClass.enums.foreach { case(enumName, enumColl) => compileEnum(enumName, enumColl) }

    out.add(extraClusterLines)
    extraClusterLines.clear()

    // Recursive types
    curClass.types.foreach { case (typeName, intClass) => compileClass(intClass) }

    out.dec
    out.puts("}")
  }

  def compileSeq(className: List[String], curClass: ClassSpec): Unit = {
    tableStart(className, "seq")
    var seqPos: Option[Int] = Some(0)
    curClass.seq.foreach { (attr) =>
      attr.id match {
        case NamedIdentifier(name) =>
          tableRow(className, seqPosToStr(seqPos), attr, name)

          val size = dataTypeBitsSize(attr.dataType)
          seqPos = (seqPos, size) match {
            case (Some(pos), Some(siz)) => Some(pos + siz)
            case _ => None
          }
      }
    }
    tableEnd
  }

  def seqPosToStr(seqPos: Option[Int]): Option[String] = {
    seqPos.map { (pos) =>
      val posByte = pos / 8
      val posBit = pos % 8
      if (posBit == 0) {
        s"$posByte"
      } else {
        s"$posByte:$posBit"
      }
    }
  }

  def compileParseInstance(className: List[String], id: InstanceIdentifier, inst: ParseInstanceSpec): Unit = {
    val name = id.name
    val lastInstPos = inst.pos
    lastInstPos match {
      case Some(pos) =>
        val posStr = expressionPos(pos, name)
        tableRow(className, Some(posStr), inst, name)

      case None =>
        tableRow(className, None, inst, name)
    }
  }

  val HEADER_BGCOLOR = "#E0FFE0"
  val SWITCH_HEADER_BGCOLOR = "#F0F2E4"
  val TH_START = "<TD BGCOLOR=\"" + HEADER_BGCOLOR + "\">"
  val SWITCH_TH_START = "<TD BGCOLOR=\"" + SWITCH_HEADER_BGCOLOR + "\">"

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

  val STYLE_EDGE_TYPE = "style=bold"
  val STYLE_EDGE_MISC = "color=\"#404040\""
  val STYLE_EDGE_POS = STYLE_EDGE_MISC
  val STYLE_EDGE_SIZE = STYLE_EDGE_MISC
  val STYLE_EDGE_REPEAT = STYLE_EDGE_MISC
  val STYLE_EDGE_VALUE = STYLE_EDGE_MISC

  def tableRow(curClass: List[String], pos: Option[String], attr: AttrLikeSpec, name: String): Unit = {
    val dataType = attr.dataType
    val sizeStr = dataTypeSizeAsString(dataType, name)

    val dataTypeStr = dataType match {
      case st: SwitchType =>
        compileSwitch(name, st)
        s"switch (${expressionType(st.on, name)})"
      case _ =>
        dataTypeName(dataType)
    }

    out.puts("<TR>" +
      "<TD PORT=\"" + name + "_pos\">" + pos.getOrElse("...") + "</TD>" +
      "<TD PORT=\"" + name + "_size\">" + sizeStr + "</TD>" +
      s"<TD>$dataTypeStr</TD>" +
      "<TD PORT=\"" + name + "_type\">" + name + "</TD>" +
      "</TR>")

    // Add user type links
    dataType match {
      case ut: UserType =>
        links += ((s"$currentTable:${name}_type", type2class(ut.name) + "__seq", STYLE_EDGE_TYPE))
      case _ =>
        // ignore, no links
    }

    val portName = name + "__repeat"
    attr.cond.repeat match {
      case RepeatExpr(ex) =>
        out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat " +
          expression(ex, s"$currentTable:$portName", STYLE_EDGE_REPEAT) +
          " times</TD></TR>")
      case RepeatUntil(ex) =>
        provider._currentIteratorType = Some(dataType)
        out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat until " +
          expression(ex, s"$currentTable:$portName", STYLE_EDGE_REPEAT) +
          "</TD></TR>")
      case RepeatEos =>
        out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat to end of stream</TD></TR>")
      case NoRepeat =>
      // no additional line
    }
  }

  def tableValueInstance(curClass: List[String], name: String, inst: ValueInstanceSpec): Unit = {
    currentTable = s"${type2class(curClass)}__inst__$name"

    out.puts(s"$currentTable" + " [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">")
    out.inc
    out.puts(s"<TR>${TH_START}id</TD>${TH_START}value</TD></TR>")

    out.puts(
      s"<TR><TD>$name</TD>" +
      "<TD>" +
      expression(inst.value, currentTable, STYLE_EDGE_VALUE) +
      "</TD></TR>"
    )

    tableEnd
  }

  def compileSwitch(attrName: String, st: SwitchType): Unit = {

    links += ((s"$currentTable:${attrName}_type", s"${currentTable}_${attrName}_switch", STYLE_EDGE_TYPE))
    extraClusterLines.puts(s"${currentTable}_${attrName}_switch " + "[label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">")
    extraClusterLines.inc
    extraClusterLines.puts(s"<TR>${SWITCH_TH_START}case</TD>${SWITCH_TH_START}type</TD></TR>")

    var lineNum = 0
    st.cases.foreach { case (caseExpr, caseType) =>
      caseType match {
        case ut: UserType =>
          val exprStr = htmlEscape(translator.translate(caseExpr))
          val portName = s"case$lineNum"
          lineNum += 1
          extraClusterLines.puts(
            "<TR><TD>" + exprStr + "</TD>" + "" +
              "<TD PORT=\"" + portName + "\">" + type2display(ut.name) + "</TD></TR>"
          )
          links += ((s"${currentTable}_${attrName}_switch:$portName", type2class(ut.name) + "__seq", STYLE_EDGE_TYPE))
        case _ =>
          // ignore, no links
      }
    }

    extraClusterLines.dec
    extraClusterLines.puts("</TABLE>>];")
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
      case FloatMultiType(width, _) => width.width.toString
      case FixedBytesType(contents, _) => contents.length.toString
      case BytesEosType(_) => END_OF_STREAM
      case BytesLimitType(ex, _) => expressionSize(ex, attrName)
      case StrByteLimitType(ex, _) => expressionSize(ex, attrName)
      case StrEosType(_) => END_OF_STREAM
      case _: StrZType => UNKNOWN
      case UserTypeByteLimit(_, ex, _) => expressionSize(ex, attrName)
      case _: UserTypeEos => END_OF_STREAM
      case UserTypeInstream(_) => UNKNOWN
      case EnumType(_, basedOn) => dataTypeSizeAsString(basedOn, attrName)
      case _: SwitchType => UNKNOWN
      case BitsType1 => "1b"
      case BitsType(width) => s"${width}b"
    }
  }

  def expressionSize(ex: expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_size", STYLE_EDGE_SIZE)
  }

  def expressionPos(ex: expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_pos", STYLE_EDGE_POS)
  }

  def expressionType(ex: expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_type", STYLE_EDGE_VALUE)
  }

  def expression(e: expr, portName: String, style: String): String = {
    affectedVars(e).foreach((v) =>
      links += ((v, portName, style))
    )
    htmlEscape(translator.translate(e))
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
      case expr.IntNum(_) | expr.FloatNum(_) | expr.Str(_) | expr.Bool(_) =>
        List()
      case expr.EnumByLabel(enumName, label) =>
        List()
      case expr.EnumById(enumName, id) =>
        affectedVars(id)
      case expr.Attribute(value, attr) =>
        val targetClass = translator.detectType(value)
        targetClass match {
          case t: UserType =>
            // Although, technically, in a clause like "foo.bar" both "foo" and
            // "bar" seem to be affecting the result, graphs seems to be more
            // readable if we'll only use "bar" for referencing, without doing
            // distinct link to all intermediate path walking nodes.

            // Uncomment this one to get "affected" references to all
            // intermediate nodes
            //affectedVars(value) ++ List(resolveTypedNode(t, attr.name))

            List(resolveTypedNode(t, attr.name))
          case _ =>
            affectedVars(value)
        }
      case expr.Subscript(value, idx) =>
        affectedVars(value) ++ affectedVars(idx)
      case SwitchType.ELSE_CONST =>
        // "_" is a special const for
        List()
      case expr.Name(Ast.identifier("_io")) =>
        // "_io" is a special const too
        List()
      case expr.Name(id) =>
        List(resolveLocalNode(id.name))
      case expr.List(elts) =>
        elts.flatMap(affectedVars).toList
    }
  }

//  def resolveNode(s: String): String = {
//    s"$currentTable:${s}_type"
//  }

  /**
    * Given a local name that should be pertinent to current class, resolve it into
    * full-blown Graphviz port reference (i.e. given something `foo` should yield
    * `stuff__seq:foo_type` is `foo` is a sequence element)
    *
    * @param s name to resolve
    * @return
    */
  def resolveLocalNode(s: String): String =
    resolveNodeForClass(nowClassName, nowClass, s)

  def resolveTypedNode(t: UserType, s: String): String = {
    val className = t.name
    val classSpec = t.classSpec.get
    resolveNodeForClass(className, classSpec, s)
  }

  def resolveNodeForClass(className: List[String], cs: ClassSpec, s: String): String =
    s"${getGraphvizNode(className, cs, s)}:${s}_type"

  def getGraphvizNode(className: List[String], cs: ClassSpec, s: String): String = {
    cs.seq.foreach((attr) =>
      attr.id match {
        case NamedIdentifier(attrName) =>
          if (attrName == s) {
            return s"${type2class(className)}__seq"
          }
      }
    )

    cs.instances.get(InstanceIdentifier(s)).foreach((inst) =>
      return s"${type2class(className)}__inst__$s"
    )

    throw new RuntimeException(s"unable to resolve node '$s' in type '${type2display(className)}'")
  }
}

object GraphvizClassCompiler extends LanguageCompilerStatic {
  override def indent: String = "\t"
  override def outFileName(topClassName: String): String = s"$topClassName.dot"
  override def getTranslator(tp: TypeProvider): BaseTranslator = new RubyTranslator(tp)

  def type2class(name: List[String]) = name.last
  def type2display(name: List[String]) = name.map(Utils.upperCamelCase).mkString("::")

  /**
    * Determines how many bits occupies given data type.
    *
    * @param dataType data type to analyze
    * @return number of bits or None, if it's impossible to determine a priori
    */
  def dataTypeBitsSize(dataType: BaseType): Option[Int] = {
    dataType match {
      case BitsType1 => Some(1)
      case BitsType(width) => Some(width)
      case EnumType(_, basedOn) => dataTypeBitsSize(basedOn)
      case _ => dataTypeByteSize(dataType).map((byteSize) => byteSize * 8)
    }
  }

  /**
    * Determines how many bytes occupies a given data type.
    *
    * @param dataType data type to analyze
    * @return number of bytes or None, if it's impossible to determine a priori
    */
  def dataTypeByteSize(dataType: BaseType): Option[Int] = {
    dataType match {
      case _: Int1Type => Some(1)
      case IntMultiType(_, width, _) => Some(width.width)
      case FixedBytesType(contents, _) => Some(contents.length)
      case FloatMultiType(width, _) => Some(width.width)
      case BytesEosType(_) => None
      case BytesLimitType(ex, _) => evaluateIntLiteral(ex)
      case StrByteLimitType(ex, _) => evaluateIntLiteral(ex)
      case StrEosType(_) => None
      case _: StrZType => None
      case UserTypeByteLimit(_, ex, _) => evaluateIntLiteral(ex)
      case _: UserTypeEos => None
      case UserTypeInstream(_) => None
      case _: SwitchType => None
    }
  }

  def dataTypeName(dataType: BaseType): String = {
    dataType match {
      case rt: ReadableType => rt.apiCall
      case ut: UserType => type2display(ut.name)
      case FixedBytesType(contents, _) => contents.map(_.formatted("%02X")).mkString(" ")
      case _: BytesType => ""
      case StrByteLimitType(_, encoding) => s"str($encoding)"
      case StrEosType(encoding) => s"str($encoding)"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        val args = ListBuffer(encoding)
        if (terminator != 0)
          args += s"term=$terminator"
        if (include)
          args += "include"
        if (!consume)
          args += "don't consume"
        if (!eosError)
          args += "ignore EOS"
        s"strz(${args.mkString(", ")})"
      case EnumType(name, basedOn) =>
        s"${dataTypeName(basedOn)}→${type2display(name)}"
      case BitsType(width) => s"b$width"
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

  def htmlEscape(s: String): String = {
    s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;")
  }
}
