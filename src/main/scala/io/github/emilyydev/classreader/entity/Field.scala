package io.github.emilyydev.classreader.entity

import io.github.emilyydev.classreader.accessflag.{AccessFlag, AccessFlagHolder, EnumAccessFlag, FinalAccessFlag, PrivateAccessFlag, ProtectedAccessFlag, PublicAccessFlag, StaticAccessFlag, SyntheticAccessFlag, TransientAccessFlag, VolatileAccessFlag}
import io.github.emilyydev.classreader.attribute.Attribute
import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.DataInput

final case class Field(
  accessFlagSet: Set[AccessFlag],
  nameIndex: Int,
  descriptorIndex: Int,
  attributes: Map[String, Attribute]
)

object Field extends AccessFlagHolder {
  def read(in: DataInput, constantPool: ConstantPool): Field = {
    val accessFlagSet = ToAccessFlagSet(in.readUnsignedShort())
    val nameIndex = in.readUnsignedShort()
    val descriptorIndex = in.readUnsignedShort()
    val attributeCount = in.readUnsignedShort()
    val attributes = for {
      _ <- 0 until attributeCount
    } yield Attribute.read(in, constantPool)
    Field(accessFlagSet, nameIndex, descriptorIndex, attributes.toMap)
  }

  override val LegalFlags: Set[AccessFlag] = Set(
    PublicAccessFlag,
    PrivateAccessFlag,
    ProtectedAccessFlag,
    StaticAccessFlag,
    FinalAccessFlag,
    VolatileAccessFlag,
    TransientAccessFlag,
    SyntheticAccessFlag,
    EnumAccessFlag
  )
}
