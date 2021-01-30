package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.precompile.{EnumNotFoundError, FieldNotFoundError, TypeNotFoundError, TypeUndecidedError}
import io.kaitai.struct.translators.TypeProvider

class ClassTypeProvider(classSpecs: ClassSpecs, var topClass: ClassSpec) extends TypeProvider {
  var nowClass = topClass

  var _currentIteratorType: Option[DataType] = None
  var _currentSwitchType: Option[DataType] = None
  def currentIteratorType: DataType = _currentIteratorType.get
  def currentSwitchType: DataType = _currentSwitchType.get

  override def determineType(attrName: String): DataType = {
    determineType(nowClass, attrName)
  }

  override def determineType(inClass: ClassSpec, attrName: String): DataType = {
    attrName match {
      case Identifier.ROOT =>
        topClass.toDataType
      case Identifier.PARENT =>
        if (inClass.parentClass == UnknownClassSpec)
          throw new RuntimeException(s"Unable to derive ${Identifier.PARENT} type in ${inClass.name.mkString("::")}")
        inClass.parentClass.toDataType
      case Identifier.IO =>
        KaitaiStreamType
      case Identifier.ITERATOR =>
        currentIteratorType
      case Identifier.SWITCH_ON =>
        currentSwitchType
      case Identifier.INDEX =>
        CalcIntType
      case Identifier.SIZEOF =>
        CalcIntType
      case _ =>
        inClass.seq.foreach { el =>
          if (el.id == NamedIdentifier(attrName))
            return el.dataTypeComposite
        }
        inClass.params.foreach { el =>
          if (el.id == NamedIdentifier(attrName))
            return el.dataType
        }
        inClass.instances.get(InstanceIdentifier(attrName)) match {
          case Some(i: ValueInstanceSpec) =>
            val dt = i.dataTypeOpt match {
              case Some(t) => t
              case None => throw new TypeUndecidedError(attrName)
            }
            return dt
          case Some(i: ParseInstanceSpec) => return i.dataTypeComposite
          case None => // do nothing
        }
        throw new FieldNotFoundError(attrName, inClass)
    }
  }

  /**
    * Attempts to resolve a member (seq attribute, parameter, instance) by given name.
    * @param inClass type specification to search member in
    * @param attrName name of a member to search for
    * @return member spec if found, or throws an exception
    */
  def resolveMember(inClass: ClassSpec, attrName: String): MemberSpec = {
    inClass.seq.foreach { el =>
      if (el.id == NamedIdentifier(attrName))
        return el
    }
    inClass.params.foreach { el =>
      if (el.id == NamedIdentifier(attrName))
        return el
    }
    inClass.instances.get(InstanceIdentifier(attrName)) match {
      case Some(i: ValueInstanceSpec) =>
        return i
      case Some(i: ParseInstanceSpec) =>
        return i
      case None => // do nothing
    }
    throw new FieldNotFoundError(attrName, inClass)
  }

  override def resolveEnum(inType: Ast.typeId, enumName: String): EnumSpec =
    resolveEnum(resolveClassSpec(inType), enumName)

  def resolveEnum(inClass: ClassSpec, enumName: String): EnumSpec = {
    inClass.enums.get(enumName) match {
      case Some(spec) =>
        spec
      case None =>
        // let's try upper levels of hierarchy
        inClass.upClass match {
          case Some(upClass) => resolveEnum(upClass, enumName)
          case None =>
            throw new EnumNotFoundError(enumName, nowClass)
        }
    }
  }

  override def resolveType(typeName: Ast.typeId): DataType =
    resolveClassSpec(typeName).toDataType

  def resolveClassSpec(typeName: Ast.typeId): ClassSpec =
    resolveClassSpec(
      if (typeName.absolute) topClass else nowClass,
      typeName.names
    )

  def resolveClassSpec(inClass: ClassSpec, typeName: Seq[String]): ClassSpec = {
    if (typeName.isEmpty)
      return inClass

    val headTypeName :: restTypesNames = typeName.toList
    val nextClass = resolveClassSpec(inClass, headTypeName)
    if (restTypesNames.isEmpty) {
      nextClass
    } else {
      resolveClassSpec(nextClass, restTypesNames)
    }
  }

  def resolveClassSpec(inClass: ClassSpec, typeName: String): ClassSpec = {
    if (inClass.name.last == typeName)
      return inClass

    inClass.types.get(typeName) match {
      case Some(spec) =>
        spec
      case None =>
        // let's try upper levels of hierarchy
        inClass.upClass match {
          case Some(upClass) => resolveClassSpec(upClass, typeName)
          case None =>
            classSpecs.get(typeName) match {
              case Some(spec) => spec
              case None =>
                throw new TypeNotFoundError(typeName, nowClass)
            }
        }
    }
  }

  override def isLazy(attrName: String): Boolean = isLazy(nowClass, attrName)

  def isLazy(inClass: ClassSpec, attrName: String): Boolean = {
    inClass.seq.foreach { el =>
      if (el.id == NamedIdentifier(attrName))
        return false
    }
    inClass.params.foreach { el =>
      if (el.id == NamedIdentifier(attrName))
        return false
    }
    inClass.instances.get(InstanceIdentifier(attrName)) match {
      case Some(i) =>
        return true
      case None =>
        // do nothing
    }
    throw new FieldNotFoundError(attrName, inClass)
  }
}
