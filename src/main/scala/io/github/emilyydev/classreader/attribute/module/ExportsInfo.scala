package io.github.emilyydev.classreader.attribute.module

import io.github.emilyydev.classreader.accessflag.{AccessFlag, MandatedAccessFlag, SyntheticAccessFlag}

import java.io.DataInput

final case class ExportsInfo(
  exportsPackageIndex: Int,
  accessFlagSet: Set[AccessFlag],
  exportsToModuleIndexes: Array[Int]
)

object ExportsInfo {
  def read(in: DataInput): ExportsInfo = {
    val exportsPackageIndex = in.readUnsignedShort()
    val accessFlagSet = AccessFlag.asSet(LegalFlags)(in.readUnsignedShort())
    val exportsToModuleCount = in.readUnsignedShort()
    val exportsToModuleIndexes = (0 until exportsToModuleCount).map(_ => in.readUnsignedShort())
    ExportsInfo(
      exportsPackageIndex,
      accessFlagSet,
      exportsToModuleIndexes.toArray
    )
  }

  private val LegalFlags: Set[AccessFlag] = Set(
    SyntheticAccessFlag,
    MandatedAccessFlag
  )
}
