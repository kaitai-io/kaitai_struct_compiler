package io.kaitai.struct

import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{TypeMismatchError, TypeProvider, TypeUndecidedError}

class ClassTypeProvider(topClass: ClassSpec) extends TypeProvider {
  var nowClass: ClassSpec = topClass
  var possibleParentClass: Option[ClassSpec] = None

  var _currentIteratorType: Option[BaseType] = None
  var _currentSwitchType: Option[BaseType] = None
  def currentIteratorType: BaseType = _currentIteratorType.get
  def currentSwitchType: BaseType = _currentSwitchType.get

  override def determineType(attrName: String): BaseType = {
    determineType(nowClass, attrName)
  }

  override def determineType(inClass: ClassSpec, attrName: String): BaseType = {
    attrName match {
      case "_root" =>
        makeUserType(topClass)
      case "_parent" =>
        var parent = inClass.parentClass
        if ((parent == UnknownClassSpec) && (possibleParentClass != None))
          parent = possibleParentClass.get
        if (parent == UnknownClassSpec)
          throw new RuntimeException(s"Unable to derive _parent type in ${inClass.name.mkString("::")}")

        makeUserType(parent)
      case "_io" =>
        KaitaiStreamType
      case "_" =>
        currentIteratorType
      case "_on" =>
        currentSwitchType
      case _ =>
        inClass.seq.foreach { el =>
          if (el.id == NamedIdentifier(attrName))
            return el.dataTypeComposite
        }
        inClass.instances.get(InstanceIdentifier(attrName)) match {
          case Some(i: ValueInstanceSpec) =>
            i.dataType match {
              case Some(t) => t
              case None => throw new TypeUndecidedError(attrName)
            }
            return i.dataType.get
          case Some(i: ParseInstanceSpec) => return i.dataTypeComposite
          case None => // do nothing
        }
        throw new RuntimeException(s"Unable to access $attrName in ${inClass.name} context")
    }
  }

  def makeUserType(csl: ClassSpecLike): BaseType = {
    csl match {
      case GenericStructClassSpec =>
        KaitaiStructType
      case cs: ClassSpec =>
        val ut = UserTypeInstream(cs.name)
        ut.classSpec = Some(cs)
        ut
    }
  }

  override def resolveEnum(enumName: String): EnumSpec = resolveEnum(nowClass, enumName)

  def resolveEnum(inClass: ClassSpec, enumName: String): EnumSpec = {
    inClass.enums.get(enumName) match {
      case Some(spec) =>
        spec
      case None =>
        // let's try upper levels of hierarchy
        inClass.upClass match {
          case Some(upClass) => resolveEnum(upClass, enumName)
          case None =>
            throw new RuntimeException(
              s"Unable to find enum '$enumName', searching from ${nowClass.name.mkString("::")}"
            )
        }
    }
  }
}
