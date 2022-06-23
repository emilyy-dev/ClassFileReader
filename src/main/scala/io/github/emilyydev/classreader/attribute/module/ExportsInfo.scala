package io.github.emilyydev.classreader.attribute.module

import io.github.emilyydev.classreader.accessflag.module.ExportsAccessFlag

import java.io.DataInput

final case class ExportsInfo(
  exportsPackageIndex: Int,
  accessFlagSet: Set[ExportsAccessFlag],
  exportsToModuleIndexes: Array[Int]
)

object ExportsInfo {
  def read(in: DataInput): ExportsInfo = {
    val exportsPackageIndex = in.readUnsignedShort()
    val accessFlagSet = ExportsAccessFlag.asSet(in.readUnsignedShort())
    val exportsToModuleCount = in.readUnsignedShort()
    val exportsToModuleIndexes = (0 until exportsToModuleCount).map(_ => in.readUnsignedShort())
    ExportsInfo(
      exportsPackageIndex,
      accessFlagSet,
      exportsToModuleIndexes.toArray
    )
  }
}
