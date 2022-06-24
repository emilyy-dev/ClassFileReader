package io.github.emilyydev.classreader.attribute

import io.github.emilyydev.classreader.accessflag.{AccessFlag, FinalAccessFlag, MandatedAccessFlag, SyntheticAccessFlag}

import java.io.DataInput

final case class MethodParameterInfo(
  nameIndex: Int,
  accessFlagSet: Set[AccessFlag]
)

object MethodParameterInfo {
  def read(in: DataInput): MethodParameterInfo =
    MethodParameterInfo(
      in.readUnsignedShort(),
      AccessFlag.asSet(LegalFlags)(in.readUnsignedShort())
    )

  private val LegalFlags: Set[AccessFlag] = Set(
    FinalAccessFlag,
    SyntheticAccessFlag,
    MandatedAccessFlag
  )
}
