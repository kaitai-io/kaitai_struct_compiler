package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.UserType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}

class HtmlClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec) extends DocClassCompiler(classSpecs, topClass) {
  import HtmlClassCompiler._

  override def outFileName(topClass: ClassSpec): String = s"${topClass.nameAsStr}.html"

  override def indent: String = ""

  override def fileHeader(topClass: ClassSpec): Unit = {
    out.puts(
      s"""
        |<!doctype html>
        |<html lang="en">
        |  <head>
        |    <!-- Required meta tags -->
        |    <meta charset="utf-8">
        |    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
        |
        |    <!-- Bootstrap CSS -->
        |    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css" integrity="sha384-GJzZqFGwb1QTTN6wy59ffF1BuGJpLSa9DkKMp0DgiMDm4iYMj70gZWKYbI706tWS" crossorigin="anonymous">
        |
        |    <title>${type2str(topClass.name.last)} format specification</title>
        |  </head>
        |  <body>
           <div class="container">
        |  <h1>${type2str(topClass.name.last)} format specification</h1>
        |
      """.stripMargin)

    // TODO: parse & output meta/title, meta/file-extensions, etc
  }

  override def fileFooter(topClass: ClassSpec): Unit = {
    out.puts(
      """
        |  </div>
        |    <!-- Optional JavaScript -->
        |    <!-- jQuery first, then Popper.js, then Bootstrap JS -->
        |    <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
        |    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js" integrity="sha384-wHAiFfRlMFy6i5SRaxvfOCifBUQy1xHdJ/yoi7FRNXMRBu5WHdZYu1hA6ZOblgut" crossorigin="anonymous"></script>
        |    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/js/bootstrap.min.js" integrity="sha384-B0UglyR+jN6CkvvICOB2joaf5I4l3gm9GU6Hc1og6Ls7i6U/mkkaduKaBhlAXv9k" crossorigin="anonymous"></script>
        |  </body>
        |</html>
      """.stripMargin)
  }

  override def classHeader(classSpec: ClassSpec): Unit = {
    out.puts(s"<a name='${classSpec2Anchor(classSpec)}'></a>")
    out.puts(s"<$headerByIndent>Type: ${type2str(classSpec.name.last)}</$headerByIndent>")
    out.puts

    classSpec.doc.summary.foreach(summary =>
      out.puts(s"<p>$summary</p>")
    )
    out.inc
  }

  override def classFooter(classSpec: ClassSpec): Unit = {
    out.dec
  }

  override def seqHeader(classSpec: ClassSpec): Unit = {
    out.puts("<table class=\"table\">")
    out.puts("<tr><th>Offset</th><th>Size</th><th>ID</th><th>Type</th><th>Note</th></tr>")
  }

  override def seqFooter(classSpec: ClassSpec): Unit = {
    out.puts("</table>")
  }

  override def compileSeqAttr(classSpec: ClassSpec, attr: AttrSpec, seqPos: Option[Int], sizeElement: Sized, sizeContainer: Sized): Unit = {
    out.puts("<tr>")
    out.puts(s"<td>${GraphvizClassCompiler.seqPosToStr(seqPos).getOrElse("???")}</td>")
    out.puts(s"<td>...</td>")
    out.puts(s"<td>${attr.id.humanReadable}</td>")
    out.puts(s"<td>${kaitaiType2NativeType(attr.dataType)}</td>")
    out.puts(s"<td>${attr.doc.summary.getOrElse("")}</td>")
    out.puts("</tr>")
  }

  override def compileParseInstance(classSpec: ClassSpec, inst: ParseInstanceSpec): Unit = {
    out.puts(s"<p><b>Parse instance</b>: ${inst.id.humanReadable}</p>")
    out.puts("<table class=\"table\">")
    out.puts("<tr>")
    out.puts(s"<td>${expression(inst.pos)}</td>")
    out.puts(s"<td>...</td>")
    out.puts(s"<td>${inst.id.humanReadable}</td>")
    out.puts(s"<td>${kaitaiType2NativeType(inst.dataType)}</td>")
    out.puts(s"<td>${inst.doc.summary.getOrElse("")}</td>")
    out.puts("</tr>")
    out.puts("</table>")
  }

  override def compileValueInstance(vis: ValueInstanceSpec): Unit = {
    out.puts(s"value instance: ${vis}")
  }

  override def compileEnum(enumName: String, enumColl: EnumSpec): Unit = {
    out.puts(s"<a name='${enumSpec2Anchor(enumColl)}'></a>")
    out.puts(s"<$headerByIndent>Enum: $enumName</$headerByIndent>")
    out.puts

    out.puts("<table class=\"table\">")
    out.puts("<tr>")
    out.puts("<th>ID</th><th>Name</th><th>Note</th>")
    out.puts("</tr>")

    enumColl.map.foreach { case (id, value) =>
      out.puts("<tr>")
      out.puts(s"<td>$id</td><td>${value.name}</td><td>${value.doc.summary.getOrElse("")}</td></tr>")
      out.puts("</tr>")
    }

    out.puts("</table>")
  }

  def headerByIndent: String = s"h${out.indentLevel + 1}"

  def expression(exOpt: Option[Ast.expr]): String = {
    exOpt match {
      case Some(ex) => translator.translate(ex)
      case None => ""
    }
  }
}

object HtmlClassCompiler extends LanguageCompilerStatic {
  // FIXME: Unused, should be probably separated from LanguageCompilerStatic
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = ???

  def type2str(name: String): String = Utils.upperCamelCase(name)

  def classSpec2Anchor(spec: ClassSpec): String = "type-" + spec.name.mkString("-")

  def enumSpec2Anchor(spec: EnumSpec): String = "enum-" + spec.name.mkString("-")

  def kaitaiType2NativeType(attrType: DataType): String = attrType match {
    case ut: UserType =>
      "<a href=\"#" + classSpec2Anchor(ut.classSpec.get) + "\">" + type2str(ut.name.last) + "</a>"
    case _ => GraphvizClassCompiler.dataTypeName(attrType, None)
  }
}
