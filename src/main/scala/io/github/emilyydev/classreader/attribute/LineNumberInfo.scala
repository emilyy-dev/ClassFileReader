package io.github.emilyydev.classreader.attribute

import java.io.DataInput

final case class LineNumberInfo(
  startingBytecodeIndex: Int,
  lineNumber: Int
)

object LineNumberInfo {
  def read(in: DataInput): LineNumberInfo =
    LineNumberInfo(in.readUnsignedShort(), in.readUnsignedShort())
}
