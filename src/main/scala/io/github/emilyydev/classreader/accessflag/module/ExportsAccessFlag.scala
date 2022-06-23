package io.github.emilyydev.classreader.accessflag.module

import io.github.emilyydev.classreader.accessflag.AccessFlag

sealed trait ExportsAccessFlag

case object SyntheticExportsFlag extends ExportsAccessFlag
case object MandatedExportsFlag extends ExportsAccessFlag

object ExportsAccessFlag extends AccessFlag[ExportsAccessFlag] {

  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_MANDATED: Int = 0x8000

  override val FlagMap: Map[Int, ExportsAccessFlag] = {
    val builder = Map.newBuilder[Int, ExportsAccessFlag]
    builder += ACC_SYNTHETIC -> SyntheticExportsFlag
    builder += ACC_MANDATED -> MandatedExportsFlag
    builder.result()
  }

  override val Names: Map[ExportsAccessFlag, String] = {
    val builder = Map.newBuilder[ExportsAccessFlag, String]
    builder += SyntheticExportsFlag -> "ACC_SYNTHETIC"
    builder += MandatedExportsFlag -> "ACC_MANDATED"
    builder.result()
  }
}
