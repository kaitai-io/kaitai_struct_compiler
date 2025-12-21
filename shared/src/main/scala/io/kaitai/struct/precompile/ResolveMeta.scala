package io.kaitai.struct.precompile

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.problems._

class ResolveMeta(classSpecs: ClassSpecs) extends PrecompileStep {
  override def run(): Iterable[CompilationProblem] = {
    classSpecs.flatMap { case (_, curClass) => resolveMeta(curClass, curClass.meta) }
  }

  def resolveMeta(curClass: ClassSpec, existingMeta: MetaSpec): Iterable[CompilationProblem] = {
    val meta = existingMeta ++ curClass.meta

    val seqProblems = curClass.seq.flatMap { attr => updateMember(attr, meta) }

    val instancesProblems = curClass.instances.flatMap { case (_, inst) =>
      updateMember(inst, meta)
    }

    val innerTypesProblems = curClass.types.flatMap { case (_, nestedClass) =>
      resolveMeta(nestedClass, meta)
    }

    (seqProblems ++ instancesProblems ++ innerTypesProblems).map(_.localizedInType(curClass))
  }

  def updateMember(member: MemberSpec, meta: MetaSpec): Iterable[CompilationProblem] = {
    (member.dataType match {
      case strType: StrFromBytesTypeUnknownEncoding =>
        updateMemberStr(member, meta, strType)
      case strzType: StrzType =>
        updateMemberStrz(member, meta, strzType)
      case _ =>
        None
    }).map(_.localizedInPath(member.path))
  }

  private def updateMemberStr(member: MemberSpec, meta: MetaSpec, strType: StrFromBytesTypeUnknownEncoding): Option[CompilationProblem with PathLocalizable] = {
    meta.encoding match {
      case Some(enc) =>
        member.updateDataType(StrFromBytesType(strType.bytes, enc))
        None
      case None =>
        Some(NoEncodingError())
    }
  }

  private def updateMemberStrz(member: MemberSpec, meta: MetaSpec, dataType: StrzType): Option[CompilationProblem with PathLocalizable] = {
    // Step 1: derive the encoding
    dataType.encoding.orElse(meta.encoding) match {
      case None =>
        Some(NoEncodingError())
      case Some(enc) =>
        // Step 2:
        // "strz" selects the appropriate null terminator depending on the "encoding", i.e. 2 zero
        // bytes for UTF-16*, 4 zero bytes for UTF-32* and 1 zero byte for all other encodings
        //
        // Call to CanonicalizeEncodingNames also returns a problem if encoding name is not
        // canonical, but we'll address it in a separate precompile step, so we can ignore it here.
        val (newEncoding, _) = CanonicalizeEncodingNames.canonicalizeName(enc)
        val nullTerm: Seq[Byte] = newEncoding match {
          /** @note Must be kept in sync with [[EncodingList.canonicalToAlias]] */
          case "UTF-16LE" | "UTF-16BE" => Seq(0, 0)
          case "UTF-32LE" | "UTF-32BE" => Seq(0, 0, 0, 0)
          case _ => Seq(0)
        }

        // Throw away old "terminator" placeholder and replace it with one derived from encoding
        val basedOn = updatedBytesType(dataType.bytes, nullTerm)

        // Step 3: update member data type
        member.updateDataType(StrFromBytesType(basedOn, enc))
        None
    }
  }

  private def updatedBytesType(oldByteType: BytesType, newTerminator: Seq[Byte]): BytesType =
    oldByteType match {
      case BytesEosType(_, include, padRight, process) =>
        BytesEosType(Some(newTerminator), include, padRight, process)
      case BytesLimitType(size, _, include, padRight, process) =>
        BytesLimitType(size, Some(newTerminator), include, padRight, process)
      case BytesTerminatedType(_, include, consume, eosError, process) =>
        BytesTerminatedType(newTerminator, include, consume, eosError, process)
      case _ => throw InternalCompilerError(
        s"strz type terminator resolution: attempting to set terminator on unsupported base type: $oldByteType"
      )
    }
}
