package io.github.emilyydev.classreader.attribute

import io.github.emilyydev.classreader.accessflag.{AbstractAccessFlag, AccessFlag, AccessFlagHolder, AnnotationAccessFlag, EnumAccessFlag, FinalAccessFlag, InterfaceAccessFlag, PrivateAccessFlag, ProtectedAccessFlag, PublicAccessFlag, StaticAccessFlag, SyntheticAccessFlag}

import java.io.DataInput

final case class InnerClassInfo(
  innerClassInfoIndex: Int,
  outerClassInfoIndex: Option[Int],
  innerNameIndex: Option[Int],
  accessFlagSet: Set[AccessFlag]
)

object InnerClassInfo extends AccessFlagHolder {
  def read(in: DataInput): InnerClassInfo = {
    val innerClassInfoIndex = in.readUnsignedShort()
    val outerClassInfoIndex = in.readUnsignedShort()
    val innerNameIndex = in.readUnsignedShort()
    val accessFlagSet = ToAccessFlagSet(in.readUnsignedShort())
    InnerClassInfo(
      innerClassInfoIndex,
      if (outerClassInfoIndex == 0) None else Some(outerClassInfoIndex),
      if (innerNameIndex == 0) None else Some(innerNameIndex),
      accessFlagSet
    )
  }

  override val LegalFlags: Set[AccessFlag] = Set(
    PublicAccessFlag,
    PrivateAccessFlag,
    ProtectedAccessFlag,
    StaticAccessFlag,
    FinalAccessFlag,
    InterfaceAccessFlag,
    AbstractAccessFlag,
    SyntheticAccessFlag,
    AnnotationAccessFlag,
    EnumAccessFlag
  )
}
