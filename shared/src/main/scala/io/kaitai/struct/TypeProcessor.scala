package io.kaitai.struct

import io.kaitai.struct.exprlang.DataType.{BaseType, SwitchType, UserType}
import io.kaitai.struct.format._

object TypeProcessor {
  def processTypes(topClass: ClassSpec): Unit = {
    markupClassNames(topClass)
    //deriveValueTypes
    resolveUserTypes(topClass)
    markupParentTypes(topClass)
  }

  def markupClassNames(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (nestedName: String, nestedClass) =>
      nestedClass.name = curClass.name ::: List(nestedName)
      nestedClass.upClass = Some(curClass)
      markupClassNames(nestedClass)
    }
  }

  def resolveUserTypes(curClass: ClassSpec): Unit = {
    curClass.seq.foreach((attr) => resolveUserTypeForAttr(curClass, attr))
    curClass.instances.foreach { case (instName, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec =>
          resolveUserTypeForAttr(curClass, pis)
        case _: ValueInstanceSpec =>
        // ignore all other types of instances
      }
    }

    curClass.types.foreach { case (_, nestedClass) =>
      resolveUserTypes(nestedClass)
    }
  }

  def resolveUserTypeForAttr(curClass: ClassSpec, attr: AttrLikeSpec): Unit =
    resolveUserType(curClass, attr.dataType)

  def resolveUserType(curClass: ClassSpec, dataType: BaseType): Unit = {
    dataType match {
      case ut: UserType =>
        ut.classSpec = resolveUserType(curClass, ut.name)
      case SwitchType(_, cases) =>
        cases.foreach { case (_, ut) =>
          resolveUserType(curClass, ut)
        }
      case _ =>
      // not a user type, nothing to resolve
    }
  }

  def resolveUserType(curClass: ClassSpec, typeName: List[String]): Option[ClassSpec] = {
    //    Console.println(s"resolveUserType: at ${curClass.name} doing ${typeName.mkString("|")}")
    val res = realResolveUserType(curClass, typeName)
    //    Console.println("   => " + (res match {
    //      case None => "???"
    //      case Some(x) => x.name.mkString("|")
    //    }))
    res
  }

  private def realResolveUserType(curClass: ClassSpec, typeName: List[String]): Option[ClassSpec] = {
    // First, try to do it in current class

    // If we're seeking composite name, we only have to resolve the very first
    // part of it at this stage
    val firstName :: restNames = typeName

    val resolvedHere = curClass.types.get(firstName).flatMap((nestedClass) =>
      if (restNames.isEmpty) {
        // No further names to resolve, here's our answer
        Some(nestedClass)
      } else {
        // Try to resolve recursively
        resolveUserType(nestedClass, restNames)
      }
    )

    resolvedHere match {
      case Some(_) => resolvedHere
      case None =>
        // No luck resolving here, let's try upper levels, if they exist
        curClass.upClass match {
          case Some(upClass) =>
            resolveUserType(upClass, typeName)
          case None =>
            // No luck at all
            None
        }
    }
  }

  def markupParentTypes(curClass: ClassSpec): Unit = {
    curClass.seq.foreach { attr =>
      markupParentTypesAdd(curClass, attr.dataType)
    }
    curClass.instances.foreach { case (instName, instSpec) =>
      markupParentTypesAdd(curClass, getInstanceDataType(instSpec))
    }
  }

  def markupParentTypesAdd(curClass: ClassSpec, dt: BaseType): Unit = {
    dt match {
      case userType: UserType =>
        val usedClass = userType.classSpec.get
        usedClass.parentClass match {
          case UnknownClassSpec =>
            usedClass.parentClass = curClass
            markupParentTypes(usedClass)
          case otherClass: ClassSpec =>
            if (otherClass == curClass) {
              // already done, don't do anything
            } else {
              // conflicting types, would be bad for statically typed languages
              // throw new RuntimeException(s"type '${attr.dataType}' has more than 1 conflicting parent types: ${otherName} and ${curClassName}")
              usedClass.parentClass = GenericStructClassSpec
            }
          case GenericStructClassSpec =>
          // already most generic case, do nothing
        }
      case _ => // ignore, it's standard type
    }
  }

  def getInstanceDataType(instSpec: InstanceSpec): BaseType = {
    instSpec match {
      case t: ValueInstanceSpec => t.dataType.get
      case t: ParseInstanceSpec => t.dataTypeComposite
    }
  }
}
