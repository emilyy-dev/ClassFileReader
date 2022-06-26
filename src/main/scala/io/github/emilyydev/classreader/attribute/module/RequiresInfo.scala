package io.github.emilyydev.classreader.attribute.module

import io.github.emilyydev.classreader.accessflag.{AccessFlag, AccessFlagHolder, MandatedAccessFlag, StaticPhaseAccessFlag, SyntheticAccessFlag, TransitiveAccessFlag}

import java.io.DataInput

final case class RequiresInfo(
  requiresModuleIndex: Int,
  accessFlagSet: Set[AccessFlag],
  requiresVersionIndex: Option[Int]
)

object RequiresInfo extends AccessFlagHolder {
  def read(in: DataInput): RequiresInfo = {
    val requiresModuleIndex = in.readUnsignedShort()
    val accessFlagSet = ToAccessFlagSet(in.readUnsignedShort())
    val requiresVersionsIndex = in.readUnsignedShort()
    RequiresInfo(
      requiresModuleIndex,
      accessFlagSet,
      if (requiresVersionsIndex == 0) None else Some(requiresVersionsIndex)
    )
  }

  override val LegalFlags: Set[AccessFlag] = Set(
    TransitiveAccessFlag,
    StaticPhaseAccessFlag,
    SyntheticAccessFlag,
    MandatedAccessFlag
  )
}
