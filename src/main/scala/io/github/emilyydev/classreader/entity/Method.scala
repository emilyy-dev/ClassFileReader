package io.github.emilyydev.classreader.entity

import io.github.emilyydev.classreader.accessflag.MethodAccessFlag
import io.github.emilyydev.classreader.attribute.Attribute
import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.DataInput

final case class Method(
  accessFlagSet: Set[MethodAccessFlag],
  nameIndex: Int,
  descriptorIndex: Int,
  attributes: Map[String, Attribute]
)

object Method {
  def read(in: DataInput, constantPool: ConstantPool): Method = {
    val accessFlagSet = MethodAccessFlag.asSet(in.readUnsignedShort())
    val nameIndex = in.readUnsignedShort()
    val descriptorIndex = in.readUnsignedShort()
    val attributeCount = in.readUnsignedShort()
    val attributes = (0 until attributeCount).map(_ => Attribute.read(in, constantPool))
    Method(accessFlagSet, nameIndex, descriptorIndex, attributes.toMap)
  }
}
