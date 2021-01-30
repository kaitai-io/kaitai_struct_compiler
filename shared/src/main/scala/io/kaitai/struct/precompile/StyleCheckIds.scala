package io.kaitai.struct.precompile
import io.kaitai.struct.ClassTypeProvider
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ClassSpec, ClassSpecs, MemberSpec}
import io.kaitai.struct.problems.{CompilationProblem, ProblemCoords, StyleWarningSizeLen}

class StyleCheckIds(specs: ClassSpecs, topClass: ClassSpec) extends PrecompileStep {
  val provider = new ClassTypeProvider(specs, topClass)

  override def run(): Iterable[CompilationProblem] =
    specs.mapRec(processType)

  def processType(spec: ClassSpec): Iterable[CompilationProblem] = {
    provider.nowClass = spec
    val sizeRefProblems = getSizeRefProblems(spec)
    sizeRefProblems
  }

  def getSizeRefProblems(spec: ClassSpec): Iterable[CompilationProblem] = {
    spec.seq.flatMap(attr =>
      getSizeReference(spec, attr.dataType).flatMap(sizeRefAttr => {
        val existingName = sizeRefAttr.id.humanReadable
        val goodName = s"len_${attr.id.humanReadable}"
        if (existingName != goodName) {
          Some(StyleWarningSizeLen(
            goodName,
            existingName,
            attr.id.humanReadable,
            ProblemCoords(path = Some(sizeRefAttr.path ++ List("id")))
          ))
        } else {
          None
        }
      })
    )
  }

  def getSizeReference(spec: ClassSpec, dataType: DataType): Option[MemberSpec] = {
    dataType match {
      case blt: BytesLimitType =>
        getDirectAttrReference(spec, blt.size)
      case ult: UserTypeFromBytes =>
        getSizeReference(spec, ult.bytes)
      case _ =>
        None
    }
  }

  def getDirectAttrReference(spec: ClassSpec, expr: Ast.expr): Option[MemberSpec] = {
    expr match {
      case Ast.expr.Name(attrId) =>
        Some(provider.resolveMember(spec, attrId.name))
      case _ =>
        None
    }
  }
}
