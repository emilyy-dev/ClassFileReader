package io.github.emilyydev.classreader.attribute

import io.github.emilyydev.classreader.accessflag.MethodParameterAccessFlag

import java.io.DataInput

final case class MethodParameterInfo(
  nameIndex: Int,
  accessFlagSet: Set[MethodParameterAccessFlag]
)

object MethodParameterInfo {
  def read(in: DataInput): MethodParameterInfo =
    MethodParameterInfo(
      in.readUnsignedShort(),
      MethodParameterAccessFlag.asSet(in.readUnsignedShort())
    )
}
