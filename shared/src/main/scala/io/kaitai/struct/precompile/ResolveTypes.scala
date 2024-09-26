package io.kaitai.struct.precompile

import io.kaitai.struct.{ClassTypeProvider, Log}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{ArrayType, EnumType, SwitchType, UserType}
import io.kaitai.struct.exprlang.Ast
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
  private def resolveUserTypes(curClass: ClassSpec): Iterable[CompilationProblem] = {
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

  private def resolveUserTypeForMember(curClass: ClassSpec, attr: MemberSpec): Iterable[CompilationProblem] =
    resolveUserType(curClass, attr.dataType, attr.path)

  private def resolveUserType(curClass: ClassSpec, dataType: DataType, path: List[String]): Iterable[CompilationProblem] = {
    dataType match {
      case ut: UserType =>
        val (resClassSpec, problems) = resolveUserType(curClass, ut.name, path ++ List("type"))
        ut.classSpec = resClassSpec
        problems
      case et: EnumType =>
        et.name match {
          case typePath :+ name =>
            try {
              val resolver = new ClassTypeProvider(specs, curClass)
              val ty = resolver.resolveEnum(Ast.typeId(false, typePath), name)
              Log.enumResolve.info(() => s"    => ${ty.nameAsStr}")
              et.enumSpec = Some(ty)
              None
            } catch {
              case ex: TypeNotFoundError =>
                Log.typeResolve.info(() => s"    => ??? (while resolving enum '${et.name}'): $ex")
                Log.enumResolve.info(() => s"    => ??? (enclosing type not found, enum '${et.name}'): $ex")
                Some(TypeNotFoundErr(typePath, curClass, path :+ "enum"))
              case ex: EnumNotFoundError =>
                Log.enumResolve.info(() => s"    => ??? (enum '${et.name}'): $ex")
                Some(EnumNotFoundErr(et.name, curClass, path :+ "enum"))
            }
          case _ =>
            Log.enumResolve.info(() => s"    => ??? (enum '${et.name}' without name)")
            // TODO: Maybe more specific error about empty name?
            Some(EnumNotFoundErr(et.name, curClass, path :+ "enum"))
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

  private def resolveUserType(curClass: ClassSpec, typeName: List[String], path: List[String]): (Option[ClassSpec], Option[CompilationProblem]) = {
    try {
      val resolver = new ClassTypeProvider(specs, curClass)
      val ty = resolver.resolveTypePath(curClass, typeName)
      Log.typeResolve.info(() => s"    => ${ty.nameAsStr}")
      (Some(ty), None)
    } catch {
      case _: TypeNotFoundError =>
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
    }
  }
}
