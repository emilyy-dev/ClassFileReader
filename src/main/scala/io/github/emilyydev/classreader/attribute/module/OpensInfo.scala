package io.github.emilyydev.classreader.attribute.module

import io.github.emilyydev.classreader.accessflag.{AccessFlag, AccessFlagHolder, MandatedAccessFlag, SyntheticAccessFlag}

import java.io.DataInput

final case class OpensInfo(
  opensPackageIndex: Int,
  accessFlagSet: Set[AccessFlag],
  opensToModuleIndexes: Array[Int]
)

object OpensInfo extends AccessFlagHolder {
  def read(in: DataInput): OpensInfo = {
    val opensPackageIndex = in.readUnsignedShort()
    val accessFlagSet = ToAccessFlagSet(in.readUnsignedShort())
    val opensToModuleCount = in.readUnsignedShort()
    val opensToModuleIndexes = for {
      - <- 0 until opensToModuleCount
    } yield in.readUnsignedShort()
    OpensInfo(opensPackageIndex, accessFlagSet, opensToModuleIndexes.toArray)
  }

  override val LegalFlags: Set[AccessFlag] = Set(
    SyntheticAccessFlag,
    MandatedAccessFlag
  )
}
