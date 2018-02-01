package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
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
        makeUserType(topClass)
      case Identifier.PARENT =>
        if (inClass.parentClass == UnknownClassSpec)
          throw new RuntimeException(s"Unable to derive ${Identifier.PARENT} type in ${inClass.name.mkString("::")}")
        makeUserType(inClass.parentClass)
      case Identifier.IO =>
        KaitaiStreamType
      case Identifier.ITERATOR =>
        currentIteratorType
      case Identifier.SWITCH_ON =>
        currentSwitchType
      case Identifier.INDEX =>
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
            val dt = i.dataType match {
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

  def makeUserType(csl: ClassSpecLike): DataType = {
    csl match {
      case GenericStructClassSpec =>
        KaitaiStructType
      case cs: ClassSpec =>
        val ut = UserTypeInstream(cs.name, None)
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
            throw new EnumNotFoundError(enumName, nowClass)
        }
    }
  }

  override def resolveType(typeName: String): DataType = resolveType(nowClass, typeName)

  def resolveType(inClass: ClassSpec, typeName: String): DataType = {
    if (inClass.name.last == typeName)
      return makeUserType(inClass)

    inClass.types.get(typeName) match {
      case Some(spec) =>
        makeUserType(spec)
      case None =>
        // let's try upper levels of hierarchy
        inClass.upClass match {
          case Some(upClass) => resolveType(upClass, typeName)
          case None =>
            classSpecs.get(typeName) match {
              case Some(spec) => makeUserType(spec)
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
