package io.github.emilyydev.classreader.attribute

import java.io.DataInput

final case class BootstrapMethodInfo(
  bootstrapMethodIndex: Int,
  bootstrapArgumentIndexes: Array[Int]
)

object BootstrapMethodInfo {
  def read(in: DataInput): BootstrapMethodInfo = {
    val bootstrapMethodIndex = in.readUnsignedShort()
    val bootstrapArgumentsNumber = in.readUnsignedShort()
    val bootstrapArgumentIndexes = (0 until bootstrapArgumentsNumber)
      .map(_ => in.readUnsignedShort())
    BootstrapMethodInfo(bootstrapMethodIndex, bootstrapArgumentIndexes.toArray)
  }
}
