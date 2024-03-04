package io.kaitai.struct.precompile
import io.kaitai.struct.ClassTypeProvider
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.problems._

class StyleCheckIds(specs: ClassSpecs) extends PrecompileStep {
  val provider = new ClassTypeProvider(specs, specs.firstSpec)

  override def run(): Iterable[CompilationProblem] =
    specs.mapTopLevel((_, typeSpec) => {
      provider.topClass = typeSpec
      typeSpec.mapRec(processType)
    })

  def processType(spec: ClassSpec): Iterable[CompilationProblem] = {
    provider.nowClass = spec
    val sizeRefProblems = getSizeRefProblems(spec)
    val repeatExprRefProblems = getRepeatExprRefProblems(spec)
    sizeRefProblems ++ repeatExprRefProblems
  }

  def getSizeRefProblems(spec: ClassSpec): Iterable[CompilationProblem] = {
    spec.seq.flatMap(attr => getSizeRefProblem(spec, attr)) ++
      spec.instances.flatMap { case (_, instance) => getSizeRefProblem(spec, instance) }
  }

  def getRepeatExprRefProblems(spec: ClassSpec): Iterable[CompilationProblem] = {
    spec.seq.flatMap(attr => getRepeatExprRefProblem(spec, attr)) ++
      spec.instances.flatMap { case (_, instance) =>
        instance match {
          case pis: ParseInstanceSpec =>
            getRepeatExprRefProblem(spec, pis)
          case _: ValueInstanceSpec =>
            None
        }
      }
  }

  def getSizeRefProblem(spec: ClassSpec, attr: MemberSpec): Option[CompilationProblem] = {
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
  }

  def getRepeatExprRefProblem(spec: ClassSpec, attr: AttrLikeSpec): Option[CompilationProblem] = {
    getRepeatExprReference(spec, attr).flatMap(repeatExprRefAttr => {
      val existingName = repeatExprRefAttr.id.humanReadable
      val goodName = s"num_${attr.id.humanReadable}"
      if (existingName != goodName) {
        Some(StyleWarningRepeatExprNum(
          goodName,
          existingName,
          attr.id.humanReadable,
          ProblemCoords(path = Some(repeatExprRefAttr.path ++ List("id")))
        ))
      } else {
        None
      }
    })
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

  def getRepeatExprReference(spec: ClassSpec, attr: AttrLikeSpec): Option[MemberSpec] = {
    attr.cond.repeat match {
      case RepeatExpr(expr) =>
        getDirectAttrReference(spec, expr)
      case _ =>
        None
    }
  }

  def getDirectAttrReference(spec: ClassSpec, expr: Ast.expr): Option[MemberSpec] = {
    try {
      expr match {
        case Ast.expr.Name(attrId) =>
          Some(provider.resolveMember(spec, attrId.name))
        case _ =>
          None
      }
    } catch {
      case _: InvalidIdentifier | _: FieldNotFoundError =>
        // if this is indeed an error, we have checked that and reported on precompile error check stages
        // no need to investigate a warning anymore
        None
    }
  }
}
