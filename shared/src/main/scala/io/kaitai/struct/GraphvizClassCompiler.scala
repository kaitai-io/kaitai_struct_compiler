package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}
import io.kaitai.struct.precompile.CalculateSeqSizes
import io.kaitai.struct.translators.RubyTranslator

import scala.collection.mutable.{ListBuffer, LinkedHashSet}

class GraphvizClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec) extends AbstractCompiler {
  import GraphvizClassCompiler._

  val out = new StringLanguageOutputWriter(indent)

  val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new RubyTranslator(provider)
  val links = LinkedHashSet[(String, String, String)]()
  val extraClusterLines = new StringLanguageOutputWriter(indent)

  def nowClass: ClassSpec = provider.nowClass
  def nowClassName = provider.nowClass.name
  var currentTable: String = ""

  override def compile: CompileLog.SpecSuccess = {
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

    CompileLog.SpecSuccess(
      "",
      List(CompileLog.FileSuccess(
        outFileName(topClass.nameAsStr),
        out.result
      ))
    )
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

    CalculateSeqSizes.forEachSeqAttr(curClass, (attr, seqPos, _, _) => {
      attr.id match {
        case NamedIdentifier(name) =>
          tableRow(className, seqPosToStr(seqPos), attr, name)
        case NumberedIdentifier(n) =>
          tableRow(className, seqPosToStr(seqPos), attr, s"_${NumberedIdentifier.TEMPLATE}$n")
      }
    })

    tableEnd
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
  val STYLE_EDGE_VALID = STYLE_EDGE_MISC
  val STYLE_EDGE_REPEAT = STYLE_EDGE_MISC
  val STYLE_EDGE_IF = STYLE_EDGE_MISC
  val STYLE_EDGE_VALUE = STYLE_EDGE_MISC

  def tableRow(curClass: List[String], pos: Option[String], attr: AttrLikeSpec, name: String): Unit = {
    val dataType = attr.dataType
    val sizeStr = dataTypeSizeAsString(dataType, name)

    val dataTypeStr = dataType match {
      case st: SwitchType =>
        compileSwitch(name, st)
        s"switch (${expressionType(st.on, name)})"
      case _ =>
        dataTypeName(dataType, attr.valid)
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

    dataType match {
      case blt: BytesLimitType if fixedBytes(blt, attr.valid).isDefined =>
        // No additional line for `valid` because it expresses the same simple
        // constraint as if the `contents` key were used, and therefore was
        // already displayed in the "type" column.
      case _ => validTableRow(dataType, attr.valid, name)
    }
    repeatTableRow(dataType, attr.cond.repeat, name)
    ifTableRow(attr.cond.ifExpr, name)
  }

  def validTableRow(dataType: DataType, valid: Option[ValidationSpec], name: String): Unit = {
    val portName = name + "__valid"
    val fullPortName = s"$currentTable:$portName"
    val text = valid match {
      case Some(v) =>
        v match {
          case ValidationEq(expected) =>
            s"must be equal to ${expression(expected, fullPortName, STYLE_EDGE_VALID)}"
          case ValidationMin(min) =>
            s"must be at least ${expression(min, fullPortName, STYLE_EDGE_VALID)}"
          case ValidationMax(max) =>
            s"must be at most ${expression(max, fullPortName, STYLE_EDGE_VALID)}"
          case ValidationRange(min, max) =>
            s"must be between ${expression(min, fullPortName, STYLE_EDGE_VALID)} " +
              s"and ${expression(max, fullPortName, STYLE_EDGE_VALID)}"
          case ValidationAnyOf(values) =>
            s"must be any of ${values.map(expression(_, fullPortName, STYLE_EDGE_VALID)).mkString(", ")}"
          case ValidationInEnum() =>
            "must be defined in the enum"
          case ValidationExpr(expr) =>
            provider._currentIteratorType = Some(dataType)
            s"must satisfy ${expression(expr, fullPortName, STYLE_EDGE_VALID)}"
        }
      case None => return
    }
    out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">" + text + "</TD></TR>")
  }

  def repeatTableRow(dataType: DataType, rep: RepeatSpec, name: String): Unit = {
    val portName = name + "__repeat"
    rep match {
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

  def ifTableRow(ifExpr: Option[Ast.expr], name: String): Unit = {
    val portName = name + "__if"
    ifExpr match {
      case Some(e) =>
        out.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">if " +
          expression(e, s"$currentTable:$portName", STYLE_EDGE_IF) +
          "</TD></TR>")
      case None => // ignore
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
  def dataTypeSizeAsString(dataType: DataType, attrName: String): String = {
    dataType match {
      case _: BytesEosType => END_OF_STREAM
      case blt: BytesLimitType => expressionSize(blt.size, attrName)
      case StrFromBytesType(basedOn, _, _) => dataTypeSizeAsString(basedOn, attrName)
      case utb: UserTypeFromBytes => dataTypeSizeAsString(utb.bytes, attrName)
      case EnumType(_, basedOn) => dataTypeSizeAsString(basedOn, attrName)
      case _ =>
        CalculateSeqSizes.dataTypeBitsSize(dataType) match {
          case FixedSized(n) =>
            if (n % 8 == 0) {
              s"${n / 8}"
            } else {
              s"${n}b"
            }
          case DynamicSized => UNKNOWN
          case NotCalculatedSized | StartedCalculationSized =>
            throw new RuntimeException("Should never happen: problems with CalculateSeqSizes")
        }
    }
  }

  def expressionSize(ex: Ast.expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_size", STYLE_EDGE_SIZE)
  }

  def expressionPos(ex: Ast.expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_pos", STYLE_EDGE_POS)
  }

  def expressionType(ex: Ast.expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_type", STYLE_EDGE_VALUE)
  }

  def expression(e: Ast.expr, portName: String, style: String): String = {
    affectedVars(e).foreach((v) =>
      links += ((v, portName, style))
    )
    htmlEscape(translator.translate(e))
  }

  def affectedVars(e: Ast.expr): List[String] = {
    e match {
      case Ast.expr.BoolOp(op, values) =>
        values.flatMap(affectedVars).toList
      case Ast.expr.BinOp(left, op, right) =>
        affectedVars(left) ++ affectedVars(right)
      case Ast.expr.UnaryOp(op, operand) =>
        affectedVars(operand)
      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
        affectedVars(condition) ++ affectedVars(ifTrue) ++ affectedVars(ifFalse)
      //      case expr.Dict(keys, values) =>
      case Ast.expr.Compare(left, ops, right) =>
        affectedVars(left) ++ affectedVars(right)
      case Ast.expr.IntNum(_) | Ast.expr.FloatNum(_) | Ast.expr.Str(_) | Ast.expr.Bool(_) =>
        List()
      case _: Ast.expr.EnumByLabel =>
        List()
      case Ast.expr.EnumById(_, id, _) =>
        affectedVars(id)
      case Ast.expr.Attribute(value, attr) =>
        if (attr.name == Identifier.SIZEOF) {
          val vars = value match {
            case Ast.expr.Name(id) if !id.name.startsWith("_") =>
              List(getGraphvizNode(nowClassName, nowClass, id.name) + s":${id.name}_size")
            case Ast.expr.Attribute(valueNested, attrNested) if !attrNested.name.startsWith("_") =>
              val targetClass = translator.detectType(valueNested)
              targetClass match {
                case t: UserType =>
                  val className = t.name
                  val classSpec = t.classSpec.get
                  List(getGraphvizNode(className, classSpec, attrNested.name) + s":${attrNested.name}_size")
                case _ =>
                  affectedVars(value)
              }
            case _ =>
              affectedVars(value)
          }
          return vars
        }

        // special names like "_io", "_parent", "_root"
        if (attr.name.startsWith("_"))
          return affectedVars(value)

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
      case Ast.expr.Call(func, args) =>
        val fromFunc = func match {
          case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) => affectedVars(obj)
        }
        fromFunc ::: affectedVars(Ast.expr.List(args))
      case Ast.expr.Subscript(value, idx) =>
        affectedVars(value) ++ affectedVars(idx)
      case Ast.expr.Name(id) =>
        if (id.name.startsWith("_")) {
          // other special names like "_", "_io", "_index", etc.
          List()
        } else {
          // this must be local name, resolve it
          List(resolveLocalNode(id.name))
        }
      case Ast.expr.List(elts) =>
        elts.flatMap(affectedVars).toList
      case Ast.expr.CastToType(expr, _) =>
        affectedVars(expr)
      case Ast.expr.ByteSizeOfType(_) =>
        List()
      case Ast.expr.BitSizeOfType(_) =>
        List()
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
    cs.seq.foreach { (attr) =>
      val name = attr.id match {
        case NamedIdentifier(attrName) =>
          attrName
        case NumberedIdentifier(n) =>
          s"_${NumberedIdentifier.TEMPLATE}$n"
      }
      if (name == s) {
        return s"${type2class(className)}__seq"
      }
    }

    cs.params.foreach { (attr) =>
      val name = attr.id match {
        case NamedIdentifier(attrName) =>
          attrName
        case NumberedIdentifier(n) =>
          s"_${NumberedIdentifier.TEMPLATE}$n"
      }
      if (name == s) {
        return s"${type2class(className)}__params"
      }
    }

    cs.instances.get(InstanceIdentifier(s)).foreach((inst) =>
      return s"${type2class(className)}__inst__$s"
    )

    throw new RuntimeException(s"unable to resolve node '$s' in type '${type2display(className)}'")
  }

  def indent: String = "\t"
  def outFileName(topClassName: String): String = s"$topClassName.dot"
}

object GraphvizClassCompiler extends LanguageCompilerStatic {
  // FIXME: Unused, should be probably separated from LanguageCompilerStatic
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = ???

  def type2class(name: List[String]) = name.last
  def type2display(name: List[String]) = name.map(Utils.upperCamelCase).mkString("::")

  def dataTypeName(dataType: DataType, valid: Option[ValidationSpec]): String = {
    dataType match {
      case rt: ReadableType => rt.apiCall(None) // FIXME
      case ut: UserType => type2display(ut.name)
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        val args = ListBuffer[String]()
        val termStr = terminator.map(_ & 0xff).mkString(", ")
        args += "term=" + (if (terminator.length == 1) termStr else s"[$termStr]")
        if (include)
          args += "include"
        if (!consume)
          args += "don't consume"
        if (!eosError)
          args += "ignore EOS"
        args.mkString(", ")
      case blt: BytesLimitType => fixedBytes(blt, valid).getOrElse("")
      case _: BytesType => ""
      case StrFromBytesType(basedOn, encoding, _) =>
        val bytesStr = dataTypeName(basedOn, valid)
        val comma = if (bytesStr.isEmpty) "" else ", "
        s"str($bytesStr$comma$encoding)"
      case EnumType(name, basedOn) =>
        s"${dataTypeName(basedOn, valid)}→${type2display(name)}"
      case BitsType(width, bitEndian) => s"b$width${bitEndian.toSuffix}"
      case BitsType1(bitEndian) => s"b1${bitEndian.toSuffix}→bool"
      case _ => dataType.toString
    }
  }

  private def fixedBytes(blt: BytesLimitType, valid: Option[ValidationSpec]): Option[String] = {
    valid match {
      case Some(ValidationEq(Ast.expr.List(contents)))
          if blt.size == Ast.expr.IntNum(contents.length) =>
        Some(contents.map(_ match {
          case Ast.expr.IntNum(byteVal) if byteVal >= 0x00 && byteVal <= 0xff => "%02X".format(byteVal)
          case _ => return None
        }).mkString(" "))
      case _ => None
    }
  }

  def htmlEscape(s: String): String = {
    s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;")
  }

  /**
    * Converts bit-level position into byte/bit human-readable combination.
    * @param seqPos optional number of bits
    * @return fractional human-readable string which displays "bytes:bits",
    *         akin to "minutes:seconds" time display
    */
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
}
