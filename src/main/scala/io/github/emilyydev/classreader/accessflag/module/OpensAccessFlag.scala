package io.github.emilyydev.classreader.accessflag.module

import io.github.emilyydev.classreader.accessflag.AccessFlag

sealed trait OpensAccessFlag

case object SyntheticOpensFlag extends OpensAccessFlag
case object MandatedOpensFlag extends OpensAccessFlag

object OpensAccessFlag extends AccessFlag[OpensAccessFlag] {

  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_MANDATED: Int = 0x8000

  override val FlagMap: Map[Int, OpensAccessFlag] = {
    val builder = Map.newBuilder[Int, OpensAccessFlag]
    builder += ACC_SYNTHETIC -> SyntheticOpensFlag
    builder += ACC_MANDATED -> MandatedOpensFlag
    builder.result()
  }

  override val Names: Map[OpensAccessFlag, String] = {
    val builder = Map.newBuilder[OpensAccessFlag, String]
    builder += SyntheticOpensFlag -> "ACC_SYNTHETIC"
    builder += MandatedOpensFlag -> "ACC_MANDATED"
    builder.result()
  }
}
