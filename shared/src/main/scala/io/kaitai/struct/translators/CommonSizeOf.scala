package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.UserType
import io.kaitai.struct.format.{DynamicSized, FixedSized}
import io.kaitai.struct.precompile.{CalculateSeqSizes, InternalCompilerError, TypeMismatchError}

object CommonSizeOf {
  def getByteSizeOfType(typeName: String, dataType: DataType): Int = {
    val bits = getBitsSizeOfType(typeName, dataType)

    // Bits-to-bytes, rounded up
    (bits + 7) / 8
  }

  def getBitsSizeOfType(typeName: String, dataType: DataType): Int = {
    val sizeSpec = dataType match {
      case ut: UserType =>
        ut.classSpec.get.seqSize
      case _ =>
        // TODO: handle impossible types errors
        CalculateSeqSizes.dataTypeBitsSize(dataType)
    }
    sizeSpec match {
      case FixedSized(n) =>
        n
      case DynamicSized =>
        throw new TypeMismatchError(s"unable to derive sizeof of `$typeName`: dynamic sized type")
      case other =>
        throw InternalCompilerError(s"internal compiler error: sizeSpec=$other for `$typeName`")
    }
  }
}
