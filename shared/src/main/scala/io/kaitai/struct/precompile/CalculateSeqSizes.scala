package io.kaitai.struct.precompile

import io.kaitai.struct.Log
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

class CalculateSeqSizes(specs: ClassSpecs) {
  def run(): Unit = specs.forEachRec(CalculateSeqSizes.getSeqSize)
}

object CalculateSeqSizes {
  def sizeMultiply(sizeElement: Sized, repeat: RepeatSpec) = {
    sizeElement match {
      case FixedSized(elementSize) =>
        repeat match {
          case NoRepeat =>
            sizeElement
          case RepeatExpr(expr) =>
            evaluateIntLiteral(expr) match {
              case Some(count) => FixedSized(elementSize * count)
              case None => DynamicSized
            }
          case _: RepeatUntil | RepeatEos =>
            DynamicSized
        }
      case _ => sizeElement
    }
  }

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
    * @return number of bits or None, if it's impossible to determine a priori
    */
  def dataTypeBitsSize(dataType: DataType): Sized = {
    dataType match {
      case BitsType1 => FixedSized(1)
      case BitsType(width) => FixedSized(width)
      case EnumType(_, basedOn) => dataTypeBitsSize(basedOn)
      case ut: UserTypeInstream => getSeqSize(ut.classSpec.get)
      case _ =>
        dataTypeByteSize(dataType) match {
          case FixedSized(x) => FixedSized(x * 8)
          case otherSize => otherSize
        }
    }
  }

  /**
    * Determines how many bytes occupies a given data type.
    *
    * @param dataType data type to analyze
    * @return number of bytes or None, if it's impossible to determine a priori
    */
  def dataTypeByteSize(dataType: DataType): Sized = {
    dataType match {
      case _: Int1Type => FixedSized(1)
      case IntMultiType(_, width, _) => FixedSized(width.width)
      case FixedBytesType(contents, _) => FixedSized(contents.length)
      case FloatMultiType(width, _) => FixedSized(width.width)
      case _: BytesEosType => DynamicSized
      case blt: BytesLimitType => evaluateIntLiteral(blt.size) match {
        case Some(x) => FixedSized(x)
        case None => DynamicSized
      }
      case _: BytesTerminatedType => DynamicSized
      case StrFromBytesType(basedOn, _) => dataTypeByteSize(basedOn)
      case utb: UserTypeFromBytes => dataTypeByteSize(utb.bytes)
      case st: SwitchType => DynamicSized // FIXME: it's really possible get size if st.hasSize
    }
  }

  /**
    * Evaluates the expression, if possible to get the result without introduction
    * of any variables or anything.
    *
    * @param expr expression to evaluate
    * @return integer result or None
    */
  def evaluateIntLiteral(expr: Ast.expr): Option[Int] = {
    expr match {
      case Ast.expr.IntNum(x) => Some(x.toInt)
      case _ => None
    }
  }
}
