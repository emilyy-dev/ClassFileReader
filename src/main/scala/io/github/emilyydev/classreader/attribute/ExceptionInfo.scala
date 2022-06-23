package io.github.emilyydev.classreader.attribute

import java.io.DataInput

final case class ExceptionInfo(
  startBytecodeIndex: Int,
  endBytecodeIndex: Int,
  handlerBytecodeIndex: Int,
  catchTypeIndex: Option[Int]
)

object ExceptionInfo {
  def read(in: DataInput): ExceptionInfo = {
    val startBytecodeIndex = in.readUnsignedShort()
    val endBytecodeIndex = in.readUnsignedShort()
    val handlerBytecodeIndex = in.readUnsignedShort()
    val catchTypeIndex = in.readUnsignedShort()
    ExceptionInfo(
      startBytecodeIndex,
      endBytecodeIndex,
      handlerBytecodeIndex,
      if (catchTypeIndex == 0) None else Some(catchTypeIndex)
    )
  }
}
