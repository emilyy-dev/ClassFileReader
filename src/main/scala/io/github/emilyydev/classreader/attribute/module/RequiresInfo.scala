package io.github.emilyydev.classreader.attribute.module

import io.github.emilyydev.classreader.accessflag.module.RequiresAccessFlag

import java.io.DataInput

final case class RequiresInfo(
  requiresModuleIndex: Int,
  accessFlagSet: Set[RequiresAccessFlag],
  requiresVersionIndex: Option[Int]
)

object RequiresInfo {
  def read(in: DataInput): RequiresInfo = {
    val requiresModuleIndex = in.readUnsignedShort()
    val accessFlagSet = RequiresAccessFlag.asSet(in.readUnsignedShort())
    val requiresVersionsIndex = in.readUnsignedShort()
    RequiresInfo(
      requiresModuleIndex,
      accessFlagSet,
      if (requiresVersionsIndex == 0) None else Some(requiresVersionsIndex)
    )
  }
}
