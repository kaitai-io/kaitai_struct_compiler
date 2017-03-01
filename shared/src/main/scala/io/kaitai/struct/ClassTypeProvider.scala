package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.precompile.{EnumNotFoundError, FieldNotFoundError, TypeNotFoundError, TypeUndecidedError}
import io.kaitai.struct.translators.TypeProvider

class ClassTypeProvider(topClass: ClassSpec) extends TypeProvider {
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
      case "_root" =>
        makeUserType(topClass)
      case "_parent" =>
        if (inClass.parentClass == UnknownClassSpec)
          throw new RuntimeException(s"Unable to derive _parent type in ${inClass.name.mkString("::")}")
        makeUserType(inClass.parentClass)
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
    inClass.types.get(typeName) match {
      case Some(spec) =>
        val ut = UserTypeInstream(spec.name, None)
        ut.classSpec = Some(spec)
        ut
      case None =>
        // let's try upper levels of hierarchy
        inClass.upClass match {
          case Some(upClass) => resolveType(upClass, typeName)
          case None =>
            throw new TypeNotFoundError(typeName, nowClass)
        }
    }
  }
}
