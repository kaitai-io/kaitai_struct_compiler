package io.kaitai.struct.precompile

import io.kaitai.struct.Log
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

class CalculateSeqSizes(specs: ClassSpecs) {
  def run(): Unit = {
    specs.forEachRec(CalculateSeqSizes.getSeqSize)
  }
}

object CalculateSeqSizes {
  def sizeMultiply(sizeElement: Sized, repeat: RepeatSpec) = {
    sizeElement match {
      case FixedSized(elementSize) =>
        repeat match {
          case NoRepeat =>
            sizeElement
          case RepeatExpr(expr) =>
            expr.evaluateIntConst match {
              case Some(count) => FixedSized(elementSize * count.toInt)
              case None => DynamicSized
            }
          case _: RepeatUntil | RepeatEos =>
            DynamicSized
        }
      case _ => sizeElement
    }
  }

  /**
    * Calculates size of the specified class instance for use in the `_sizeof`
    * special property and `sizeof<>` meta-function.
    *
    * @param curClass
    */
  def getSeqSize(curClass: ClassSpec): Sized = {
    curClass.seqSize match {
      case DynamicSized | _: FixedSized =>
      // do nothing, it's already calculated
      case StartedCalculationSized =>
        // recursive size dependency encountered => we won't be able to determine
        // let's break the infinite loop
        curClass.seqSize = DynamicSized
      case NotCalculatedSized =>
        // launch the calculation
        curClass.seqSize = StartedCalculationSized
        val seqSize = forEachSeqAttr(curClass, (attr, seqPos, sizeElement, sizeContainer) => {})
        curClass.seqSize = seqSize match {
          case Some(size) => FixedSized(size)
          case None => DynamicSized
        }
    }

    Log.seqSizes.info(() => s"sizeof(${curClass.nameAsStr}) = ${curClass.seqSize}")
    curClass.seqSize
  }

  /**
    * Traverses type's sequence of attributes, calling operation for every attribute.
    * Operation is called with arguments (attr, seqPos, sizeElement, sizeContainer)
    * @param curClass type specification to traverse
    * @param op operation to apply to every sequence attribute
    * @return total size of sequence, if possible (i.e. it's fixed size)
    */
  def forEachSeqAttr(curClass: ClassSpec, op: (AttrSpec, Option[Int], Sized, Sized) => Unit): Option[Int] = {
    var seqPos: Option[Int] = Some(0)
    curClass.seq.foreach { attr =>
      val sizeElement = dataTypeBitsSize(attr.dataType)
      val sizeContainer = sizeMultiply(sizeElement, attr.cond.repeat)

      op(attr, seqPos, sizeElement, sizeContainer)

      seqPos = (seqPos, sizeContainer) match {
        case (Some(pos), FixedSized(siz)) => Some(pos + siz)
        case _ => None
      }
    }
    seqPos
  }

  /**
    * Determines how many bits occupies given data type.
    *
    * @param dataType data type to analyze
    * @return number of bits or [[DynamicSized]], if it's impossible to determine a priori
    */
  def dataTypeBitsSize(dataType: DataType): Sized = {
    dataType match {
      case BitsType1(_) => FixedSized(1)
      case CalcBooleanType => DynamicSized

      case EnumType(_, basedOn) => dataTypeBitsSize(basedOn)

      case ut: UserTypeInstream => getSeqSize(ut.classSpec.get)
      case ut: UserTypeFromBytes => dataTypeBitsSize(ut.bytes)
      case ut: CalcUserTypeFromBytes => dataTypeBitsSize(ut.bytes)
      case _: StructType => DynamicSized

      case BitsType(width, _) => FixedSized(width)
      case Int1Type(_) => FixedSized(8)
      case IntMultiType(_, width, _) => FixedSized(width.width * 8)
      case CalcIntType => DynamicSized

      case FloatMultiType(width, _) => FixedSized(width.width * 8)
      case CalcFloatType => DynamicSized

      case _: BytesEosType => DynamicSized
      case blt: BytesLimitType => blt.size.evaluateIntConst match {
        case Some(x) => FixedSized(x.toInt * 8)
        case None => DynamicSized
      }
      case _: BytesTerminatedType => DynamicSized
      case CalcBytesType => DynamicSized

      case StrFromBytesType(basedOn, _, _) => dataTypeBitsSize(basedOn)
      case CalcStrType => DynamicSized

      case st: SwitchType => DynamicSized // FIXME: it's really possible get size if st.hasSize

      case OwnedKaitaiStreamType | KaitaiStreamType => DynamicSized

      // TODO: Add special type or attribute to ArrayType for arrays of known size
      case _: ArrayType => DynamicSized
      case AnyType => DynamicSized
    }
  }
}
