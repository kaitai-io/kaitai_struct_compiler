package io.kaitai.struct.precompile

import io.kaitai.struct.Log
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{ArrayType, EnumType, SwitchType, UserType}
import io.kaitai.struct.format._
import io.kaitai.struct.problems._

/**
  * A collection of methods that resolves user types and enum types, i.e.
  * converts names into ClassSpec / EnumSpec references.
  */
class ResolveTypes(specs: ClassSpecs, topClass: ClassSpec, opaqueTypes: Boolean) extends PrecompileStep {
  override def run(): Iterable[CompilationProblem] =
    topClass.mapRec(resolveUserTypes).map(problem => problem.localizedInType(topClass))

  /**
    * Resolves user types and enum types recursively starting from a certain
    * ClassSpec.
    * @param curClass class to start from, might be top-level class
    */
  def resolveUserTypes(curClass: ClassSpec): Iterable[CompilationProblem] = {
    val seqProblems: Iterable[CompilationProblem] =
      curClass.seq.flatMap((attr) => resolveUserTypeForMember(curClass, attr))

    val instancesProblems: Iterable[CompilationProblem] =
      curClass.instances.flatMap { case (_, instSpec) =>
        instSpec match {
          case pis: ParseInstanceSpec =>
            resolveUserTypeForMember(curClass, pis)
          case _: ValueInstanceSpec =>
            // ignore all other types of instances
            None
        }
      }

    val paramsProblems: Iterable[CompilationProblem] =
      curClass.params.flatMap((paramDef) => resolveUserTypeForMember(curClass, paramDef))

    seqProblems ++ instancesProblems ++ paramsProblems
  }

  def resolveUserTypeForMember(curClass: ClassSpec, attr: MemberSpec): Iterable[CompilationProblem] =
    resolveUserType(curClass, attr.dataType, attr.path)

  def resolveUserType(curClass: ClassSpec, dataType: DataType, path: List[String]): Iterable[CompilationProblem] = {
    dataType match {
      case ut: UserType =>
        val (resClassSpec, problems) = resolveUserType(curClass, ut.name, path ++ List("type"))
        ut.classSpec = resClassSpec
        problems
      case et: EnumType =>
        et.enumSpec = resolveEnumSpec(curClass, et.name)
        if (et.enumSpec.isEmpty) {
          Some(EnumNotFoundErr(et.name, curClass, path ++ List("enum")))
        } else {
          None
        }
      case st: SwitchType =>
        st.cases.flatMap { case (caseName, ut) =>
          resolveUserType(curClass, ut, path ++ List("type", "cases", caseName.toString))
        }
      case at: ArrayType =>
        resolveUserType(curClass, at.elType, path)
      case _ =>
        // not a user type, nothing to resolve
        None
    }
  }

  def resolveUserType(curClass: ClassSpec, typeName: List[String], path: List[String]): (Option[ClassSpec], Option[CompilationProblem]) = {
    val res = realResolveUserType(curClass, typeName, path)

    res match {
      case None =>
        // Type definition not found
        if (opaqueTypes) {
          // Generate special "opaque placeholder" ClassSpec
          Log.typeResolve.info(() => "    => ??? (generating opaque type)")
          (Some(ClassSpec.opaquePlaceholder(typeName)), None)
        } else {
          // Opaque types are disabled => that is an error
          Log.typeResolve.info(() => "    => ??? (opaque type are disabled => error)")
          (None, Some(TypeNotFoundErr(typeName, curClass, path)))
        }
      case Some(x) =>
        Log.typeResolve.info(() => s"    => ${x.nameAsStr}")
        (res, None)
    }
  }

  private def realResolveUserType(curClass: ClassSpec, typeName: List[String], path: List[String]): Option[ClassSpec] = {
    Log.typeResolve.info(() => s"resolveUserType: at ${curClass.name} doing ${typeName.mkString("|")}")

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
        realResolveUserType(nestedClass, restNames, path)
      }
    )

    resolvedHere match {
      case Some(_) => resolvedHere
      case None =>
        // No luck resolving here, let's try upper levels, if they exist
        curClass.upClass match {
          case Some(upClass) =>
            realResolveUserType(upClass, typeName, path)
          case None =>
            // Check this class if it's top-level class
            if (curClass.name.head == firstName) {
              Some(curClass)
            } else {
              // Check if top-level specs has this name
              // If there's None => no luck at all
              val resolvedTop = specs.get(firstName)
              resolvedTop match {
                case None => None
                case Some(classSpec) => if (restNames.isEmpty) {
                  resolvedTop
                } else {
                  realResolveUserType(classSpec, restNames, path)
                }
              }
            }
        }
    }
  }

  def resolveEnumSpec(curClass: ClassSpec, typeName: List[String]): Option[EnumSpec] = {
    Log.enumResolve.info(() => s"resolveEnumSpec: at ${curClass.name} doing ${typeName.mkString("|")}")

    val res = realResolveEnumSpec(curClass, typeName)
    res match {
      case None => {
        Log.enumResolve.info(() => s"    => ???")
        res
      }
      case Some(x) => {
        Log.enumResolve.info(() => s"    => ${x.nameAsStr}")
        res
      }
    }
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
            // Check this class if it's top-level class
            if (curClass.name.head == firstName) {
              resolveEnumSpec(curClass, restNames)
            } else {
              // Check if top-level specs has this name
              // If there's None => no luck at all
              val resolvedTop = specs.get(firstName)
              resolvedTop match {
                case None => None
                case Some(classSpec) => if (restNames.isEmpty) {
                  // resolved everything, but this points to a type name, not enum name
                  None
                } else {
                  resolveEnumSpec(classSpec, restNames)
                }
              }
            }
        }
    }
  }
}
