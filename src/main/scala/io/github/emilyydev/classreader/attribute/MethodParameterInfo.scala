package io.github.emilyydev.classreader.attribute

import io.github.emilyydev.classreader.accessflag.{AccessFlag, AccessFlagHolder, FinalAccessFlag, MandatedAccessFlag, SyntheticAccessFlag}

import java.io.DataInput

final case class MethodParameterInfo(
  nameIndex: Int,
  accessFlagSet: Set[AccessFlag]
)

object MethodParameterInfo extends AccessFlagHolder {
  def read(in: DataInput): MethodParameterInfo =
    MethodParameterInfo(
      in.readUnsignedShort(),
      ToAccessFlagSet(in.readUnsignedShort())
    )

  override val LegalFlags: Set[AccessFlag] = Set(
    FinalAccessFlag,
    SyntheticAccessFlag,
    MandatedAccessFlag
  )
}
