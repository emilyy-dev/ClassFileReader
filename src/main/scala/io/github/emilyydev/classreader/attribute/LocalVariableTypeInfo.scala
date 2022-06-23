package io.github.emilyydev.classreader.attribute

import java.io.DataInput

final case class LocalVariableTypeInfo(
  startBytecodeIndex: Int,
  length: Int,
  nameIndex: Int,
  signatureIndex: Int,
  index: Int
)

object LocalVariableTypeInfo {
  def read(in: DataInput): LocalVariableTypeInfo =
    LocalVariableTypeInfo(
      in.readUnsignedShort(),
      in.readUnsignedShort(),
      in.readUnsignedShort(),
      in.readUnsignedShort(),
      in.readUnsignedShort()
    )
}
