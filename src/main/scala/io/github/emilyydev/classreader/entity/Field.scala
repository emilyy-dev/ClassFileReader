package io.github.emilyydev.classreader.entity

import io.github.emilyydev.classreader.accessflag.FieldAccessFlag
import io.github.emilyydev.classreader.attribute.Attribute
import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.DataInput

final case class Field(
  accessFlagSet: Set[FieldAccessFlag],
  nameIndex: Int,
  descriptorIndex: Int,
  attributes: Map[String, Attribute]
)

object Field {
  def read(in: DataInput, constantPool: ConstantPool): Field = {
    val accessFlagSet = FieldAccessFlag.asSet(in.readUnsignedShort())
    val nameIndex = in.readUnsignedShort()
    val descriptorIndex = in.readUnsignedShort()
    val attributeCount = in.readUnsignedShort()
    val attributes = (0 until attributeCount).map(_ => Attribute.read(in, constantPool))
    Field(accessFlagSet, nameIndex, descriptorIndex, attributes.toMap)
  }
}
