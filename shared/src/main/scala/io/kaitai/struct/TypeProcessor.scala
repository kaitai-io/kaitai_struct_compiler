package io.kaitai.struct

import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, RubyTranslator, TypeUndecidedError}

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

object TypeProcessor {
  // We need to record the parent types of resolved user types at their usage, which we know about
  // on resolving user types, to be able to properly resolve "_parent" in value instances to solve
  // some chicken&egg problem in the current implementation.
  //
  // The key is a string by purpose currently, because on deriving value there may be some opaque
  // types and for some reasion their ClassSpec couldn't be queried as a key in the map without
  // running into UnsupportedOperationException of AbstractIdentifier.toString. This doesn't happen
  // if we provide a string using ClassSpec on our own.
  val parentsByChild = Map[String, ClassSpec]()

  def processTypes(topClass: ClassSpec): Unit = {
    // Set top class name from meta
    topClass.name = List(topClass.meta.get.id.get)
    topClass.parentClass = GenericStructClassSpec

    markupClassNames(topClass)
    resolveUserTypes(topClass)
    deriveValueTypes(topClass)
    markupParentTypes(topClass)
  }

  // ==================================================================

  def markupClassNames(curClass: ClassSpec): Unit = {
    curClass.enums.foreach { case (enumName, enumSpec) =>
      enumSpec.name = curClass.name ::: List(enumName)
    }

    curClass.types.foreach { case (nestedName: String, nestedClass) =>
      nestedClass.name = curClass.name ::: List(nestedName)
      nestedClass.upClass = Some(curClass)
      markupClassNames(nestedClass)
    }
  }

  // ==================================================================

  def deriveValueTypes(topClass: ClassSpec) {
    val provider = new ClassTypeProvider(topClass)
    // TODO: make more abstract translator subtype for type detection only
    val translator = new RubyTranslator(provider)

    var iterNum = 1
    var hasChanged = false
    do {
//      Console.println(s"... deriveValueType: iteration #$iterNum")
      hasChanged = deriveValueType(provider, translator, topClass)
      iterNum += 1
    } while (hasChanged)
  }

  def deriveValueType(provider: ClassTypeProvider, translator: BaseTranslator, curClass: ClassSpec): Boolean = {
//    Console.println(s"deriveValueType(${curClass.name.mkString("::")})")
    var hasChanged = false

    provider.nowClass = curClass
    provider.possibleParentClass = parentsByChild.get(curClass.name.mkString("::"))

    curClass.instances.foreach {
      case (instName, inst) =>
        inst match {
          case vi: ValueInstanceSpec =>
            vi.dataType match {
              case None =>
                try {
                  val viType = translator.detectType(vi.value)
                  vi.dataType = Some(viType)
//                  Console.println(s"${instName.name} derived type: $viType")
                  hasChanged = true
                } catch {
                  case tue: TypeUndecidedError =>
//                    Console.println(s"${instName.name} type undecided: ${tue.getMessage}")
                    // just ignore, we're not there yet, probably we'll get it on next iteration
                }
              case Some(_) =>
                // already derived, do nothing
            }
          case _ =>
            // do nothing
        }
    }

    // Continue with all nested types
    curClass.types.foreach {
      case (_, classSpec) =>
        hasChanged ||= deriveValueType(provider, translator, classSpec)
    }

    hasChanged
  }

  // ==================================================================

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
      case et: EnumType =>
        et.enumSpec = resolveEnumSpec(curClass, et.name)
        if (et.enumSpec.isEmpty)
          throw new RuntimeException(s"enum not found: '${et.name}'")
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

    // TODO: add some option to control whether using an unresolved type should be a error or a placeholder should be
    // generated

    res match {
      case None =>
        // Type definition not found - generate special "opaque placeholder" ClassSpec
        Some(ClassSpec.opaquePlaceholder(typeName))
      case Some(child) => {
        // We might be called multiple times, but only have two situations: It's either always the
        // same context or in case of different/incompatible ones there's some chance that the last
        // call is the proper one, so we simply overwrite mappings. Depending on the target language
        // in case of multiple incompatible usages compiler errors occur or such and things can be
        // fixed as needed.
        parentsByChild(child.name.mkString("::")) = curClass
        res
      }
    }
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
            // Check this class if it's top-level class
            if (curClass.name.head == firstName) {
              Some(curClass)
            } else {
              // No luck at all
              None
            }
        }
    }
  }

  def resolveEnumSpec(curClass: ClassSpec, typeName: List[String]): Option[EnumSpec] = {
//    Console.println(s"resolveEnumSpec: at ${curClass.name} doing ${typeName.mkString("|")}")
    val res = realResolveEnumSpec(curClass, typeName)
//    Console.println("   => " + (res match {
//      case None => "???"
//      case Some(x) => x.name.mkString("|")
//    }))

    res
  }

  private def realResolveEnumSpec(curClass: ClassSpec, typeName: List[String]): Option[EnumSpec] = {
    // First, try to do it in current class

    // If we're seeking composite name, we only have to resolve the very first
    // part of it at this stage
    val firstName :: restNames = typeName

    val resolvedHere = if (restNames.isEmpty) {
      curClass.enums.get(firstName)
    } else {
      curClass.types.get(firstName).flatMap((nestedClass) =>
        resolveEnumSpec(nestedClass, restNames)
      )
    }

    resolvedHere match {
      case Some(_) => resolvedHere
      case None =>
        // No luck resolving here, let's try upper levels, if they exist
        curClass.upClass match {
          case Some(upClass) =>
            resolveEnumSpec(upClass, typeName)
          case None =>
            // No luck at all
            None
        }
    }
  }

  // ==================================================================

  def markupParentTypes(curClass: ClassSpec): Unit = {
    curClass.seq.foreach { attr =>
      markupParentTypesAdd(curClass, attr.dataType)
    }
    curClass.instances.foreach { case (_, instSpec) =>
      markupParentTypesAdd(curClass, getInstanceDataType(instSpec))
    }
  }

  def markupParentTypesAdd(curClass: ClassSpec, dt: BaseType): Unit = {
    dt match {
      case userType: UserType =>
        markupParentAs(curClass, userType)
      case switchType: SwitchType =>
        switchType.cases.foreach {
          case (_, ut: UserType) =>
            markupParentAs(curClass, ut)
          case (_, _) =>
            // ignore everything else
        }
      case ArrayType(innerType) =>
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

  def markupParentAs(parent: ClassSpec, child: ClassSpec): Unit = {
    child.parentClass match {
      case UnknownClassSpec =>
        child.parentClass = parent
        markupParentTypes(child)
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

  def getInstanceDataType(instSpec: InstanceSpec): BaseType = {
    instSpec match {
      case t: ValueInstanceSpec => t.dataType.get
      case t: ParseInstanceSpec => t.dataTypeComposite
    }
  }

  // ==================================================================

  def getOpaqueClasses(curClass: ClassSpec): Iterable[ClassSpec] = {
    val res = ListBuffer[ClassSpec]()
    curClass.seq.map((attr) =>
      res ++= getOpaqueDataTypes(attr.dataType)
    )
    curClass.instances.foreach { case (_, inst) =>
      inst match {
        case pis: ParseInstanceSpec =>
          res ++= getOpaqueDataTypes(pis.dataType)
        case _ => None
      }
    }

    // Traverse all nested types recursively
    curClass.types.foreach { case (_, nestedType) =>
      res ++= getOpaqueClasses(nestedType)
    }

    res
  }

  def getOpaqueDataTypes(dataType: BaseType): Iterable[ClassSpec] = {
    dataType match {
      case ut: UserType =>
        if (ut.isOpaque) {
          List(ut.classSpec.get)
        } else {
          List()
        }
      case SwitchType(_, cases) =>
        cases.flatMap { case (_, ut) =>
          getOpaqueDataTypes(ut)
        }
      case _ =>
        // all other types are not opaque external user types
        List()
    }
  }
}
