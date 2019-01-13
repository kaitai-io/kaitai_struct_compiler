package io.kaitai.struct

import io.kaitai.struct.format._
import io.kaitai.struct.precompile.CalculateSeqSizes
import io.kaitai.struct.translators.RubyTranslator

abstract class DocClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec) extends AbstractCompiler {
  val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new RubyTranslator(provider)

  // TODO: move it into SingleOutputFile equivalent
  val out = new StringLanguageOutputWriter(indent)
  def outFileName(topClass: ClassSpec): String
  def indent: String
  // END move to SingleOutputFile

  def nowClass: ClassSpec = provider.nowClass
  def nowClassName = provider.nowClass.name

  override def compile: CompileLog.SpecSuccess = {
    fileHeader(topClass)
    compileClass(topClass)
    fileFooter(topClass)

    CompileLog.SpecSuccess(
      "",
      List(CompileLog.FileSuccess(
        outFileName(topClass),
        out.result
      ))
    )
  }

  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass
    val className = curClass.name

    classHeader(curClass)

    // Sequence
    compileSeq(curClass)

    curClass.instances.foreach { case (instName, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec =>
          compileParseInstance(pis)
        case vis: ValueInstanceSpec =>
          compileValueInstance(vis)
      }
    }

    curClass.enums.foreach { case(enumName, enumColl) => compileEnum(enumName, enumColl) }

    // Recursive types
    curClass.types.foreach { case (typeName, intClass) => compileClass(intClass) }

    classFooter(curClass)
  }

  def compileSeq(curClass: ClassSpec): Unit = {
    seqHeader(curClass)

    CalculateSeqSizes.forEachSeqAttr(curClass, (attr, seqPos, sizeElement, sizeContainer) => {
      compileSeqAttr(attr, seqPos, sizeElement, sizeContainer)
    })

    seqFooter(curClass)
  }

  def fileHeader(topClass: ClassSpec): Unit
  def fileFooter(topClass: ClassSpec): Unit

  def classHeader(classSpec: ClassSpec): Unit
  def classFooter(classSpec: ClassSpec): Unit

  def seqHeader(classSpec: ClassSpec): Unit
  def seqFooter(classSpec: ClassSpec): Unit

  def compileSeqAttr(attr: AttrSpec, seqPos: Option[Int], sizeElement: Sized, sizeContainer: Sized): Unit
  def compileParseInstance(inst: ParseInstanceSpec): Unit
  def compileValueInstance(vis: ValueInstanceSpec): Unit
  def compileEnum(enumName: String, enumColl: EnumSpec): Unit
}
