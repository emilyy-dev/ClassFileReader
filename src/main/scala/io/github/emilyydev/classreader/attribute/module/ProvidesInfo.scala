package io.github.emilyydev.classreader.attribute.module

import java.io.DataInput

final case class ProvidesInfo(
  serviceIndex: Int,
  serviceProviderIndexes: Array[Int]
)

object ProvidesInfo {
  def read(in: DataInput): ProvidesInfo = {
    val serviceIndex = in.readUnsignedShort()
    val serviceProviderCount = in.readUnsignedShort()
    val serviceProviderIndexes= (0 until serviceProviderCount).map(_ => in.readUnsignedShort())
    ProvidesInfo(serviceIndex, serviceProviderIndexes.toArray)
  }
}
