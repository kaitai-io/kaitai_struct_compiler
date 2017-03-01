package io.kaitai.struct.precompile

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{EnumType, SwitchType, UserType}
import io.kaitai.struct.format._

/**
  * A collection of methods that resolves user types and enum types, i.e.
  * converts names into ClassSpec / EnumSpec references.
  */
object ResolveTypes {
  /**
    * Resolves user types and enum types recursively starting from a certain
    * ClassSpec.
    * @param curClass class to start from, might be top-level class
    */
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

  def resolveUserType(curClass: ClassSpec, dataType: DataType): Unit = {
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
      case Some(x) =>
        res
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
}
