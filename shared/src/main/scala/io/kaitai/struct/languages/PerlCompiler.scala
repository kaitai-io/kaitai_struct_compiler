package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{PerlTranslator, TypeProvider}
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig}

class PerlCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
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

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.pm"

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"# $headerComment")
    out.puts
    out.puts("use strict;")
    out.puts("use warnings;")
    out.puts(s"use $packageName ${KSVersion.minimalRuntime.toPerlVersion};")
    out.puts("use Compress::Zlib;")
    out.puts("use Encode;")
    out.puts("use List::Util;")
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
    out.puts(s"return new($$class, $kstreamName->new($$fd));")
    universalFooter
  }

  override def classFooter(name: List[String]): Unit = {}

  override def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit = {
    out.puts
    out.puts("sub new {")
    out.inc
    out.puts("my ($class, $_io, $_parent, $_root) = @_;")
    out.puts(s"my $$self = $kstructName->new($$_io);")
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

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, condSpec: ConditionalSpec): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, condSpec: ConditionalSpec): Unit = {
    attrName match {
      case RootIdentifier | ParentIdentifier =>
        // ignore, they are already defined in KaitaiStruct class
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
      case RepeatEos => s"$memberName[-1]"
      case RepeatExpr(_) => s"$memberName[$$i]"
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
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

  override def alignToByte(io: String): Unit =
    out.puts(s"$io->align_to_byte();")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = ();")
    out.puts(s"${privateMemberName(id)} = ();")
    out.puts(s"while (!$io->is_eof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"push @{${privateMemberName(id)}}, $expr;")

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = {
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

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = ();")
    out.puts(s"${privateMemberName(id)} = ();")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (decl, tmpName) = if (isRaw) {
      ("my ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }
    out.puts(s"$decl$tmpName = $expr;")
    out.puts(s"push @{${privateMemberName(id)}}, $tmpName;")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} until (${expression(untilExpr)});")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr;")

  override def parseExpr(dataType: DataType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read_${t.apiCall}()"
      case blt: BytesLimitType =>
        s"$io->read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io->read_bytes_full()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io->read_bytes_term($terminator, ${boolLiteral(include)}, ${boolLiteral(consume)}, ${boolLiteral(eosError)})"
      case BitsType1 =>
        s"$io->read_bits_int(1)"
      case BitsType(width: Int) =>
        s"$io->read_bits_int($width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(fp) => translator.translate(fp)
            case None => "$self"
          }
          s", $parent, ${privateMemberName(RootIdentifier)}"
        }
        s"${types2class(t.classSpec.get.name)}->new($io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName::bytes_terminate($expr1, $term, ${boolLiteral(include)})"
      case None => expr1
    }
    expr2
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

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType): Unit = {
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
  override def getTranslator(tp: TypeProvider, config: RuntimeConfig) = new PerlTranslator(tp)
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new PerlCompiler(tp, config)

  def packageName: String = "IO::KaitaiStruct"
  override def kstreamName: String = s"$packageName::Stream"
  override def kstructName: String = s"$packageName::Struct"
}
