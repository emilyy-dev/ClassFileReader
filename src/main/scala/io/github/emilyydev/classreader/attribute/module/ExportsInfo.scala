package io.github.emilyydev.classreader.attribute.module

import io.github.emilyydev.classreader.accessflag.{AccessFlag, AccessFlagHolder, MandatedAccessFlag, SyntheticAccessFlag}

import java.io.DataInput

final case class ExportsInfo(
  exportsPackageIndex: Int,
  accessFlagSet: Set[AccessFlag],
  exportsToModuleIndexes: Array[Int]
)

object ExportsInfo extends AccessFlagHolder {
  def read(in: DataInput): ExportsInfo = {
    val exportsPackageIndex = in.readUnsignedShort()
    val accessFlagSet = ToAccessFlagSet(in.readUnsignedShort())
    val exportsToModuleCount = in.readUnsignedShort()
    val exportsToModuleIndexes = for {
      _ <- 0 until exportsToModuleCount
    } yield in.readUnsignedShort()
    ExportsInfo(
      exportsPackageIndex,
      accessFlagSet,
      exportsToModuleIndexes.toArray
    )
  }

  override val LegalFlags: Set[AccessFlag] = Set(
    SyntheticAccessFlag,
    MandatedAccessFlag
  )
}
