package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, RuntimeConfig}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.{UserType, _}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{NoNeedForFullClassPath, _}
import io.kaitai.struct.translators.{BaseTranslator, PerlTranslator, TypeProvider}

class PerlCompiler(config: RuntimeConfig, out: LanguageOutputWriter)
  extends LanguageCompiler(config, out)
    with UniversalFooter
    with UpperCamelCaseClasses
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
    with EveryReadIsExpression {

  import PerlCompiler._

  override def innerClasses = false

  override def getStatic: LanguageCompilerStatic = PerlCompiler

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"# $headerComment")
    out.puts
    out.puts("use strict;")
    out.puts("use warnings;")
    out.puts(s"use $kstructName;")
    out.puts(s"use $kstreamName;")
    out.puts("use Compress::Zlib;")
  }

  override def fileFooter(topClassName: String): Unit = {
    out.puts
    out.puts("1;")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit =
    out.puts(s"use ${type2class(classSpec.name.head)};")

  override def classHeader(name: List[String]): Unit = {
    out.puts
    out.puts("########################################################################")
    out.puts(s"package ${types2class(name)};")
    out.puts
    out.puts(s"our @ISA = '$kstructName';")
    out.puts
    out.puts("sub from_file {")
    out.inc
    out.puts("my ($class, $filename) = @_;")
    out.puts("my $fd;")
    out.puts
    out.puts("open($fd, '<', $filename) or return undef;")
    out.puts("binmode($fd);")
    out.puts("return new($class, Kaitai::Stream->new($fd));")
    universalFooter
  }

  override def classFooter(name: List[String]): Unit = {}

  override def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit = {
    out.puts
    out.puts("sub new {")
    out.inc
    out.puts("my ($class, $_io, $_parent, $_root) = @_;")
    out.puts("my $self = Kaitai::Struct->new($_io);")
    out.puts
    out.puts("bless $self, $class;")
    out.puts(s"${privateMemberName(ParentIdentifier)} = $$_parent;")
    out.puts(s"${privateMemberName(RootIdentifier)} = $$_root || $$self;")
    out.puts
  }

  override def classConstructorFooter(): Unit = {
    out.puts
    out.puts("return $self;")
    universalFooter
  }

  override def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    attrName match {
      case RootIdentifier | ParentIdentifier =>
        // ignore, they are already defined in Kaitai::Struct class
      case _ =>
        out.puts
        out.puts(s"sub ${publicMemberName(attrName)} {")
        out.inc

        out.puts("my ($self) = @_;")
        out.puts(s"return ${privateMemberName(attrName)};")

        universalFooter
    }
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {
    out.puts(s"${privateMemberName(attrName)} = $normalIO->ensure_fixed_contents($contents);")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    out.puts(proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"$destName = $kstreamName::$procName($srcName, ${expression(xorValue)});"
      case ProcessZlib =>
        s"$destName = Compress::Zlib::uncompress($srcName);"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$destName = $kstreamName::process_rotate_left($srcName, $expr, 1);"
    })
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatEos | RepeatUntil(_) => s"$memberName[-1]"
      case RepeatExpr(_) => s"$memberName[$$i]"
      case NoRepeat => s"$memberName"
    }

    val ioName = s"$$io_${idToStr(id)}"

    out.puts(s"my $ioName = $kstreamName->new($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"my $$io = ${expression(ioEx)};")
    "$io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"my $$_pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io->seek($$_pos);")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = ();")
    out.puts(s"${privateMemberName(id)} = ();")
    out.puts(s"while (!$io->is_eof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"push @{${privateMemberName(id)}}, $expr;")

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = ();")
    out.puts(s"${privateMemberName(id)} = ();")
    val nVar = s"$$n_${idToStr(id)}"
    out.puts(s"my $nVar = ${expression(repeatExpr)};")
    out.puts(s"for (my $$i = 0; $$i < $nVar; $$i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[$$i] = $expr;")

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = ();")
    out.puts(s"${privateMemberName(id)} = ();")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String): Unit = {
    out.puts(s"${translator.doName("_")} = $expr;")
    out.puts(s"push @{${privateMemberName(id)}}, ${translator.doName("_")};")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} until (${expression(untilExpr)});")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr;")

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read_${t.apiCall}()"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io->read_str_byte_limit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + "->read_str_eos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + "->read_strz(\"" + encoding + '"' + s", $terminator, ${boolLiteral(include)}, ${boolLiteral(consume)}, ${boolLiteral(eosError)})"
      case BytesLimitType(size, _) =>
        s"$io->read_bytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io->read_bytes_full()"
      case t: UserType =>
        val addArgs = if (!t.isOpaque) s", $$self, ${privateMemberName(RootIdentifier)}" else ""
        s"${types2class(t.classSpec.get.name)}->new($io$addArgs)"
    }
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    typeProvider._currentSwitchType = Some(translator.detectType(on))
    out.puts(s"my $$_on = ${expression(on)};")
  }

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if (${expression(onComparisonExpr(condition))}) {")
    out.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elsif (${expression(onComparisonExpr(condition))}) {")
    out.inc
  }

  override def switchCaseEnd(): Unit = universalFooter

  override def switchElseStart(): Unit = {
    out.puts(s"else {")
    out.inc
  }

  override def switchEnd(): Unit = {}

  /**
    * Generates comparison expression by a given condition expression, comparing
    * it to special local variable "_on".
    * @param condition condition to check for equality with "_on"
    * @return comparison expression of boolean type
    */
  def onComparisonExpr(condition: Ast.expr) =
    Ast.expr.Compare(Ast.expr.Name(Ast.identifier("_on")), Ast.cmpop.Eq, condition)

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: BaseType): Unit = {
    out.puts
    out.puts(s"sub ${instName.name} {")
    out.inc
    out.puts("my ($self) = @_;")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)} if (${privateMemberName(instName)});")
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    out.puts

    enumColl.foreach { case (id, label) =>
      out.puts(s"our ${enumValue(enumName, label)} = $id;")
    }
  }

  def enumValue(enumName: String, enumLabel: String) = translator.doEnumByLabel(List(enumName), enumLabel)

  override def idToStr(id: Identifier): String = {
    id match {
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case si: SpecialIdentifier => si.name
      case RawIdentifier(inner) => s"_raw_${idToStr(inner)}"
      case InstanceIdentifier(name) => name
    }
  }

  override def privateMemberName(id: Identifier): String = s"$$self->{${idToStr(id)}}"

  override def publicMemberName(id: Identifier): String = idToStr(id)

  def boolLiteral(b: Boolean): String = translator.doBoolLiteral(b)

  def types2class(t: List[String]) = t.map(type2class).mkString("::")
}

object PerlCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new PerlTranslator(tp)
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.pm"
  override def indent: String = "    "

  override def kstreamName: String = "Kaitai::Stream"
  override def kstructName: String = "Kaitai::Struct"
}
