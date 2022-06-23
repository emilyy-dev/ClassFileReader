package io.github.emilyydev.classreader.attribute

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.DataInput

final case class RecordComponentInfo(
  nameIndex: Int,
  descriptorIndex: Int,
  attributes: Map[String, Attribute]
)

object RecordComponentInfo {
  def read(in: DataInput, constantPool: ConstantPool): RecordComponentInfo = {
    val nameIndex = in.readUnsignedShort()
    val descriptorIndex = in.readUnsignedShort()
    val attributeCount = in.readUnsignedShort()
    val attributes = (0 until attributeCount).map(_ => Attribute.read(in, constantPool))
    RecordComponentInfo(nameIndex, descriptorIndex, attributes.toMap)
  }
}
