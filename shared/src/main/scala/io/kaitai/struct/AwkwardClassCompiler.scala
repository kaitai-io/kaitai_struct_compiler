package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, _}
import io.kaitai.struct.languages.AwkwardCompiler
import io.kaitai.struct.languages.components.{ExtraAttrs, LanguageCompiler, LanguageCompilerStatic}

class AwkwardClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, AwkwardCompiler) {

  val awk = new AwkwardCompiler(provider, config)

  /**
    * Generates code for one full class using a given [[format.ClassSpec]].
    * @param curClass current class to generate code for
    */
  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    curClass.meta.imports.foreach(file => lang.importFile(file))

    lang.createBuilderMap(curClass)

    if (!lang.innerDocstrings)
      compileClassDoc(curClass)
    lang.classHeader(curClass.name)
    if (lang.innerDocstrings)
      compileClassDoc(curClass)

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    // Forward declarations for params which reference types external to this type
    curClass.params.foreach((paramDefSpec) =>
      paramDefSpec.dataType match {
        case ut: UserType =>
          val externalTypeName = ut.classSpec.get.name
          if (externalTypeName.head != curClass.name.head) {
            lang.classForwardDeclaration(externalTypeName)
          }
        case _ => // no forward declarations needed
      }
    )

    if (lang.innerEnums)
      compileEnums(curClass)

    if (lang.config.readStoresPos)
      lang.debugClassSequence(curClass.seq)

    // Constructor
    compileConstructor(curClass)

    // Read method(s)
    compileEagerRead(curClass.seq, curClass.meta.endian)

    // Destructor
    compileDestructor(curClass)

    // Recursive types
    if (lang.innerClasses) {
      compileSubclasses(curClass)

      provider.nowClass = curClass
    }

    compileInstances(curClass)

    // Attributes declarations and readers
    val allAttrs: List[MemberSpec] =
      curClass.seq ++
      curClass.params ++
      List(
        AttrSpec(List(), RootIdentifier, CalcUserType(topClassName, None)),
        AttrSpec(List(), ParentIdentifier, curClass.parentType)
      ) ++
      ExtraAttrs.forClassSpec(curClass, lang)
    compileAttrDeclarations(allAttrs)
    compileAttrReaders(allAttrs)

    curClass.toStringExpr.foreach(expr => lang.classToString(expr))

    lang.classFooter(curClass.name)

    if (!lang.innerClasses)
      compileSubclasses(curClass)

    if (!lang.innerEnums)
      compileEnums(curClass)
  }

  override def compileConstructor(curClass: ClassSpec) = {
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )
    compileInit(curClass)
    curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
    if (lang.config.autoRead)
      lang.runRead(curClass.name)
    lang.classConstructorFooter
  }
}