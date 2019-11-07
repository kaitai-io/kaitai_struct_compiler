package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.CalcUserType
import io.kaitai.struct.format.{ClassSpec, DynamicSized, FixedSized}
import io.kaitai.struct.precompile.{CalculateSeqSizes, InternalCompilerError, TypeMismatchError}

object CommonSizeOf {
  /**
    * Converts bit size to byte size, rounded up to byte margin.
    * @param bits number of bits
    * @return number of bytes it takes
    */
  def bitToByteSize(bits: Int): Int = (bits + 7) / 8

  def getBitsSizeOfType(typeName: String, dataType: DataType): Int = {
    val sizeSpec = dataType match {
      case cut: CalcUserType => cut.classSpec.get.seqSize
      case _ => CalculateSeqSizes.dataTypeBitsSize(dataType)
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

  def getByteSizeOfClassSpec(cs: ClassSpec): Int = {
    bitToByteSize(cs.seqSize match {
      case FixedSized(n) => n
      case DynamicSized =>
        throw new TypeMismatchError(s"unable to derive sizeof for type `${cs.nameAsStr}`: dynamic sized type")
      case other =>
        throw InternalCompilerError(s"internal compiler error: sizeSpec=$other for type `${cs.nameAsStr}`")
    })
  }
}
