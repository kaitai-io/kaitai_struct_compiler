package io.kaitai.struct.precompile

import io.kaitai.struct.EncodingList
import io.kaitai.struct.datatype.DataType.StrFromBytesType
import io.kaitai.struct.format._
import io.kaitai.struct.precompile.CanonicalizeEncodingNames._
import io.kaitai.struct.problems._

class CanonicalizeEncodingNames(specs: ClassSpecs) extends PrecompileStep {
  override def run(): Iterable[CompilationProblem] = specs.mapRec(canonicalize)

  def canonicalize(curClass: ClassSpec): Iterable[CompilationProblem] = {
    val metaProblems = canonicalizeMeta(curClass.meta)
    val seqProblems = curClass.seq.flatMap(attr => canonicalizeMember(attr))
    val instanceProblems = curClass.instances.flatMap { case (_, attr) => canonicalizeMember(attr) }
    (metaProblems ++ seqProblems ++ instanceProblems).map(problem => problem.localizedInType(curClass))
  }

  def canonicalizeMeta(meta: MetaSpec): Iterable[CompilationProblem] = {
    (meta.encoding match {
      case Some(encoding) =>
        val (newEncoding, problem1) = canonicalizeName(encoding)
        meta.encoding = Some(newEncoding)
        problem1
      case None =>
        // no encoding = no problems
        None
    }).map(problem => problem.localizedInPath(meta.path ++ List("encoding")))
  }

  def canonicalizeMember(member: MemberSpec): Iterable[CompilationProblem] = {
    (member.dataType match {
      case strType: StrFromBytesType =>
        val (newEncoding, problem1) = canonicalizeName(strType.encoding)
        strType.encoding = newEncoding
        problem1
      case _ =>
        // not a string type = no problem
        None
    }).map(problem => problem.localizedInPath(member.path ++ List("encoding")))
  }
}

object CanonicalizeEncodingNames {
  def canonicalizeName(original: String, unrecognizedIsError: Boolean = true): (String, Option[CompilationProblem with PathLocalizable]) = {
    // Try exact match with canonical list
    if (EncodingList.canonicalToAlias.contains(original)) {
      (original, None)
    } else {
      // See if any aliases match
      aliasToCanonical.get(original.toUpperCase) match {
        case Some(canonical) =>
          (
            canonical,
            Some(EncodingNameWarning(canonical, original))
          )
        case None =>
          (
            original,
            Some(UnrecognizedEncodingError(original))
          )
      }
    }
  }

  private val aliasToCanonical: Map[String, String] =
    EncodingList.canonicalToAlias.flatMap { case (canonical, aliases) =>
      aliases.map(alias => (alias.toUpperCase, canonical))
    } ++
      EncodingList.canonicalToAlias.keys.map(x => x.toUpperCase -> x)
}
