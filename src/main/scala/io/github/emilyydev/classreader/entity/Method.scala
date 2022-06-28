package io.github.emilyydev.classreader.entity

import io.github.emilyydev.classreader.accessflag.{AbstractAccessFlag, AccessFlag, AccessFlagHolder, BridgeAccessFlag, FinalAccessFlag, NativeAccessFlag, PrivateAccessFlag, ProtectedAccessFlag, PublicAccessFlag, StaticAccessFlag, StrictAccessFlag, SynchronizedAccessFlag, SyntheticAccessFlag, VarargsAccessFlag}
import io.github.emilyydev.classreader.attribute.Attribute
import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.DataInput

final case class Method(
  accessFlagSet: Set[AccessFlag],
  nameIndex: Int,
  descriptorIndex: Int,
  attributes: Map[String, Attribute]
)

object Method extends AccessFlagHolder {
  def read(in: DataInput, constantPool: ConstantPool): Method = {
    val accessFlagSet = ToAccessFlagSet(in.readUnsignedShort())
    val nameIndex = in.readUnsignedShort()
    val descriptorIndex = in.readUnsignedShort()
    val attributeCount = in.readUnsignedShort()
    val attributes = for {
      _ <- 0 until attributeCount
    } yield Attribute.read(in, constantPool)
    Method(accessFlagSet, nameIndex, descriptorIndex, attributes.toMap)
  }

  override val LegalFlags: Set[AccessFlag] = Set(
    PublicAccessFlag,
    PrivateAccessFlag,
    ProtectedAccessFlag,
    StaticAccessFlag,
    FinalAccessFlag,
    SynchronizedAccessFlag,
    BridgeAccessFlag,
    VarargsAccessFlag,
    NativeAccessFlag,
    AbstractAccessFlag,
    StrictAccessFlag,
    SyntheticAccessFlag
  )
}
