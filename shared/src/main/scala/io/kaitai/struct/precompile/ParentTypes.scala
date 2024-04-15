package io.kaitai.struct.precompile

import io.kaitai.struct.{ClassTypeProvider, Log}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{ArrayTypeInStream, SwitchType, UserType}
import io.kaitai.struct.format._
import io.kaitai.struct.translators.TypeDetector

/**
  * Precompile step that calculates actual parent types of KSY-defined types
  * (the type of the `_parent` built-in property).
  */
class ParentTypes(classSpecs: ClassSpecs) {
  def run(): Unit = {
    classSpecs.foreach { case (_, curClass) => markup(curClass) }
    classSpecs.forEachTopLevel((_, spec) => {
      spec.parentClass = GenericStructClassSpec
    })
  }

  def markup(curClass: ClassSpec): Unit = {
    Log.typeProcParent.info(() => s"ParentTypes.markup(${curClass.nameAsStr})")

    if (curClass.seq.nonEmpty)
      Log.typeProcParent.info(() => s"... seq")
    curClass.seq.foreach { attr =>
      markupParentTypesAdd(curClass, attr.dataType)
    }

    if (curClass.instances.nonEmpty)
      Log.typeProcParent.info(() => s"... instances")
    curClass.instances.foreach { case (_, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec =>
          markupParentTypesAdd(curClass, pis.dataTypeComposite)
        case _: ValueInstanceSpec =>
          // value instances have no effect on parenting, just do nothing
      }
    }

    if (curClass.types.nonEmpty)
      Log.typeProcParent.info(() => s"... types")
    curClass.types.foreach { case (_, ty) =>
      // If parent is not decided yet, calculate it
      if (ty.parentClass == UnknownClassSpec) {
        markup(ty)
      }
    }
  }

  /** Calculates `parent` of `dt` */
  private
  def markupParentTypesAdd(curClass: ClassSpec, dt: DataType): Unit = {
    dt match {
      case userType: UserType =>
        userType.forcedParent match {
          // `parent` key is not specified in attribute
          case None =>
            markupParentAs(curClass, userType)
          // `parent: false` specified in attribute
          case Some(DataType.USER_TYPE_NO_PARENT) =>
            Log.typeProcParent.info(() => s"..... no parent type added")
          // `parent: <expression>` specified in attribute
          case Some(parent) =>
            val provider = new ClassTypeProvider(classSpecs, curClass)
            val detector = new TypeDetector(provider)
            val parentType = detector.detectType(parent)
            Log.typeProcParent.info(() => s"..... enforced parent type = $parentType")
            parentType match {
              case ut: UserType =>
                markupParentAs(ut.classSpec.get, userType)
              case other =>
                throw new TypeMismatchError(s"parent=$parent is expected to be either of user type or `false`, but $other found")
            }
        }
      case switchType: SwitchType =>
        switchType.cases.foreach {
          case (_, ut: UserType) =>
            markupParentAs(curClass, ut)
          case (_, _) =>
            // ignore everything else
        }
      case ArrayTypeInStream(innerType) =>
        markupParentTypesAdd(curClass, innerType)
      case _ => // ignore, it's standard type
    }
  }

  def markupParentAs(curClass: ClassSpec, ut: UserType): Unit = {
    ut.classSpec match {
      case Some(usedClass) =>
        markupParentAs(curClass, usedClass)
      case None =>
        // TODO: replace with proper warning API
        Console.println(s"warning: tried to mark up parent=${curClass.name} for user type ${ut.name.mkString("::")}, but that type wasn't found, so doing nothing");
    }
  }

  /**
    * If parent of `child` is not calculated yet, makes `parent` the parent
    * type. Otherwise, if `parent` is different from existing parent, replaces
    * parent type with the most generic KS type for user types.
    */
  def markupParentAs(parent: ClassSpec, child: ClassSpec): Unit = {
    // Don't allow type usages across spec boundaries to affect parent resolution
    if (child.isExternal(parent)) {
      Log.typeProcParent.info(() => s"..... cross-spec usage of class=${child.nameAsStr} from parent=${parent.nameAsStr} ignored")
      return
    }
    Log.typeProcParent.info(() => s"..... class=${child.nameAsStr} has parent=${parent.nameAsStr}")
    child.parentClass match {
      case UnknownClassSpec =>
        child.parentClass = parent
        markup(child)
      case otherClass: ClassSpec =>
        if (otherClass == parent) {
          // already done, don't do anything
        } else {
          // conflicting types, would be bad for statically typed languages
          // throw new RuntimeException(s"type '${attr.dataType}' has more than 1 conflicting parent types: ${otherName} and ${curClassName}")
          child.parentClass = GenericStructClassSpec
        }
      case GenericStructClassSpec =>
        // already most generic case, do nothing
    }
  }
}
