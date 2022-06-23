package io.github.emilyydev.classreader.attribute.module

import io.github.emilyydev.classreader.accessflag.module.OpensAccessFlag

import java.io.DataInput

final case class OpensInfo(
  opensPackageIndex: Int,
  accessFlagSet: Set[OpensAccessFlag],
  opensToModuleIndexes: Array[Int]
)

object OpensInfo {
  def read(in: DataInput): OpensInfo = {
    val opensPackageIndex = in.readUnsignedShort()
    val accessFlagSet = OpensAccessFlag.asSet(in.readUnsignedShort())
    val opensToModuleCount = in.readUnsignedShort()
    val opensToModuleIndexes = (0 until opensToModuleCount).map(_ => in.readUnsignedShort())
    OpensInfo(opensPackageIndex, accessFlagSet, opensToModuleIndexes.toArray)
  }
}
