package io.github.emilyydev.classreader.attribute

import io.github.emilyydev.classreader.accessflag.InnerClassAccessFlag

import java.io.DataInput

final case class InnerClassInfo(
  innerClassInfoIndex: Int,
  outerClassInfoIndex: Option[Int],
  innerNameIndex: Option[Int],
  accessFlagSet: Set[InnerClassAccessFlag]
)

object InnerClassInfo {
  def read(in: DataInput): InnerClassInfo = {
    val innerClassInfoIndex = in.readUnsignedShort()
    val outerClassInfoIndex = in.readUnsignedShort()
    val innerNameIndex = in.readUnsignedShort()
    val accessFlagSet = InnerClassAccessFlag.asSet(in.readUnsignedShort())
    InnerClassInfo(
      innerClassInfoIndex,
      if (outerClassInfoIndex == 0) None else Some(outerClassInfoIndex),
      if (innerNameIndex == 0) None else Some(innerNameIndex),
      accessFlagSet
    )
  }
}
