package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.precompile.{EnumNotFoundError, ExpressionError, FieldNotFoundError, TypeNotFoundError, TypeUndecidedError}
import io.kaitai.struct.translators.TypeProvider

class ClassTypeProvider(classSpecs: ClassSpecs, var topClass: ClassSpec) extends TypeProvider {
  var nowClass = topClass
  val allClasses: ClassSpecs = classSpecs

  /**
    * Type of the `_` variable in the expression. That variable is defined in
    * `repeat-until` and `valid: expr` contexts and refers to the attribute
    * just parsed.
    */
  var _typeOfUnderscore: Option[DataType] = None
  /**
    * Type of the `_on` variable in the expression. That variable is defined in
    * `cases.<case>` contexts and refers to the value of `switch-on` expression.
    */
  var _currentSwitchType: Option[DataType] = None

  override def determineType(attrName: String): DataType = {
    determineType(nowClass, attrName)
  }

  override def determineType(attrId: Identifier): DataType = {
    determineType(nowClass, attrId)
  }

  override def determineType(inClass: ClassSpec, attrName: String): DataType = {
    attrName match {
      case Identifier.ROOT =>
        topClass.toDataType
      case Identifier.PARENT =>
        if (inClass.parentClass == UnknownClassSpec)
          throw new ExpressionError(s"Unable to derive '${Identifier.PARENT}' type in '${inClass.nameAsStr}'")
        inClass.parentClass.toDataType
      case Identifier.IO =>
        KaitaiStreamType
      case Identifier.ITERATOR =>
        _typeOfUnderscore match {
          case Some(value) => value
          case None => throw new ExpressionError(s"Context variable '$attrName' is available only in the 'repeat-until' and 'valid/expr' attributes")
        }
      case Identifier.SWITCH_ON =>
        _currentSwitchType match {
          case Some(value) => value
          case None => throw new ExpressionError(s"Context variable '$attrName' is available only in the 'cases.<case>' expressions")
        }
      case Identifier.INDEX =>
        CalcIntType
      case Identifier.SIZEOF =>
        CalcIntType
      case _ =>
        resolveMember(inClass, attrName).dataTypeComposite
    }
  }

  override def determineType(inClass: ClassSpec, attrId: Identifier): DataType = {
    attrId match {
      case _: NumberedIdentifier =>
        // only 'seq' fields can be unnamed (numbered)
        inClass.seq.foreach { el =>
          if (el.id == attrId)
            return el.dataTypeComposite
        }
      case NamedIdentifier(name) => return determineType(inClass, name)
      case InstanceIdentifier(name) => return determineType(inClass, name)
      case SpecialIdentifier(name) => return determineType(inClass, name)
      case RawIdentifier(innerId) => {
        val innerType = determineType(innerId)
        val (isArray, itemType: DataType) = innerType match {
          case at: ArrayType => (true, at.elType)
          case t => (false, t)
        }
        val singleType: DataType = itemType match {
          case st: SwitchType => st.cases.collectFirst {
            case (_, caseType)
              if caseType.isInstanceOf[BytesType]
              || caseType.isInstanceOf[UserTypeFromBytes] => caseType
          }.get
          case t => t
        }
        /** see [[languages.components.ExtraAttrs$]] for possible types */
        val bytesType = singleType match {
          case bt: BytesType => bt
          case utb: UserTypeFromBytes => utb.bytes
        }
        return if (isArray) ArrayTypeInStream(bytesType) else bytesType
      }
      case OuterSizeIdentifier(innerId) =>
        val singleType = CalcIntType
        return if (determineType(innerId).isInstanceOf[ArrayType]) ArrayTypeInStream(singleType) else singleType
      case InnerSizeIdentifier(innerId) =>
        val singleType = CalcIntType
        return if (determineType(innerId).isInstanceOf[ArrayType]) ArrayTypeInStream(singleType) else singleType
      case _ => // do nothing
    }
    throw new FieldNotFoundError(attrId.humanReadable, inClass)
  }

  /**
    * Attempts to resolve a member (seq attribute, parameter, instance) by given name.
    * @param inClass type specification to search member in
    * @param attrName name of a member to search for
    * @return member spec if found, or throws an exception
    * @throws format.InvalidIdentifier if attribute name is not a valid name for a member
    * @throws precompile.FieldNotFoundError if attribute with such name is not found
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
