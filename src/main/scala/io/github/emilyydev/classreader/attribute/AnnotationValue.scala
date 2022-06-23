package io.github.emilyydev.classreader.attribute

import java.io.DataInput

sealed trait AnnotationValue

final case class ByteElementValue(valueIndex: Int) extends AnnotationValue
final case class CharElementValue(valueIndex: Int) extends AnnotationValue
final case class DoubleElementValue(valueIndex: Int) extends AnnotationValue
final case class FloatElementValue(valueIndex: Int) extends AnnotationValue
final case class IntElementValue(valueIndex: Int) extends AnnotationValue
final case class LongElementValue(valueIndex: Int) extends AnnotationValue
final case class ShortElementValue(valueIndex: Int) extends AnnotationValue
final case class BooleanElementValue(valueIndex: Int) extends AnnotationValue
final case class StringElementValue(valueIndex: Int) extends AnnotationValue
final case class EnumElementValue(typeNameIndex: Int, nameIndex: Int) extends AnnotationValue
final case class ClassElementValue(classInfoIndex: Int) extends AnnotationValue
final case class AnnotationElementValue(value: AnnotationInfo) extends AnnotationValue
final case class ArrayElementValue(values: List[AnnotationValue]) extends AnnotationValue

object AnnotationValue {

  val ByteType: Int = 'B'
  val CharType: Int = 'C'
  val DoubleType: Int = 'D'
  val FloatType: Int = 'F'
  val IntType: Int = 'I'
  val LongType: Int = 'J'
  val ShortType: Int = 'S'
  val BooleanType: Int = 'Z'
  val StringType: Int = 's'
  val EnumType: Int = 'e'
  val ClassType: Int = 'c'
  val AnnotationType: Int = '@'
  val ArrayType: Int = '['

  def read(in: DataInput): AnnotationValue = {
    in.readUnsignedByte() match {
      case ByteType => ByteElementValue(in.readUnsignedShort())
      case CharType => CharElementValue(in.readUnsignedShort())
      case DoubleType => DoubleElementValue(in.readUnsignedShort())
      case FloatType => FloatElementValue(in.readUnsignedShort())
      case IntType => IntElementValue(in.readUnsignedShort())
      case LongType => LongElementValue(in.readUnsignedShort())
      case ShortType => ShortElementValue(in.readUnsignedShort())
      case BooleanType => BooleanElementValue(in.readUnsignedShort())
      case StringType => StringElementValue(in.readUnsignedShort())
      case EnumType => EnumElementValue(in.readUnsignedShort(), in.readUnsignedShort())
      case ClassType => ClassElementValue(in.readUnsignedShort())
      case AnnotationType => AnnotationElementValue(AnnotationInfo.read(in))
      case ArrayType =>
        val valuesNumber = in.readUnsignedShort()
        val values = (0 until valuesNumber).map(_ => AnnotationValue.read(in))
        ArrayElementValue(values.toList)
    }
  }
}
