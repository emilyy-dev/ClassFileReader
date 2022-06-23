package io.github.emilyydev.classreader.attribute

import java.io.DataInput

sealed trait LocalVariableInfo {
  val startBytecodeIndex: Int
  val length: Int
  val nameIndex: Int
  val descriptorIndex: Int
  val index: Int
}

final case class ValidLocalVariableInfo(
  override val startBytecodeIndex: Int,
  override val length: Int,
  override val nameIndex: Int,
  override val descriptorIndex: Int,
  override val index: Int
) extends LocalVariableInfo

case object DummyLocalVariableInfo extends LocalVariableInfo {
  override val startBytecodeIndex: Int = 0
  override val length: Int = 0
  override val nameIndex: Int = 0
  override val descriptorIndex: Int = 0
  override val index: Int = 0
}

object LocalVariableInfo {
  def read(in: DataInput): LocalVariableInfo =
    ValidLocalVariableInfo(
      in.readUnsignedShort(),
      in.readUnsignedShort(),
      in.readUnsignedShort(),
      in.readUnsignedShort(),
      in.readUnsignedShort()
    )
}
