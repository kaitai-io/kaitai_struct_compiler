package io.kaitai.struct.precompile

import io.kaitai.struct.EncodingList
import io.kaitai.struct.datatype.DataType.StrFromBytesType
import io.kaitai.struct.format._
import io.kaitai.struct.precompile.CanonicalizeEncodingNames._
import io.kaitai.struct.problems._
import io.kaitai.struct.Platform

class CanonicalizeEncodingNames(specs: ClassSpecs) extends PrecompileStep {
  override def run(): Iterable[CompilationProblem] = specs.mapRec(canonicalize)
}

object CanonicalizeEncodingNames {
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
        // Do not report problem if encoding was derived from `meta/encoding` key
        if (strType.isEncodingDerived) None else problem1
      case _ =>
        // not a string type = no problem
        None
    }).map(problem => problem.localizedInPath(member.path ++ List("encoding")))
  }

  def canonicalizeName(original: String): (String, Option[CompilationProblem with PathLocalizable]) = {
    // Try exact match with canonical list
    if (EncodingList.canonicalToAlias.contains(original)) {
      (original, None)
    } else {
      // See if any aliases match
      aliasToCanonical.get(Platform.toUpperLocaleInsensitive(original)) match {
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
      aliases.map(alias => (Platform.toUpperLocaleInsensitive(alias), canonical))
    } ++
      EncodingList.canonicalToAlias.keys.map(x => Platform.toUpperLocaleInsensitive(x) -> x)
}
