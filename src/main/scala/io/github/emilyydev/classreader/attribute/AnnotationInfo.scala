package io.github.emilyydev.classreader.attribute

import java.io.DataInput

final case class AnnotationInfo(
  typeIndex: Int,
  elementValuePairs: List[AnnotationElementValuePair]
)

final case class AnnotationElementValuePair(
  nameIndex: Int,
  value: AnnotationValue
)

object AnnotationInfo {
  def read(in: DataInput): AnnotationInfo = {
    val typeIndex = in.readUnsignedShort()
    val elementValuePairsNumber = in.readUnsignedShort()
    val elementValuePairs = (0 until elementValuePairsNumber)
      .map(_ => AnnotationElementValuePair(in.readUnsignedShort(), AnnotationValue.read(in)))
    AnnotationInfo(typeIndex, elementValuePairs.toList)
  }
}
