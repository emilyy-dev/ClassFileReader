package io.github.emilyydev.classreader.accessflag.module

import io.github.emilyydev.classreader.accessflag.AccessFlag

sealed trait RequiresAccessFlag

case object TransitiveRequiresFlag extends RequiresAccessFlag
case object StaticPhaseRequiresFlag extends RequiresAccessFlag
case object SyntheticRequiresFlag extends RequiresAccessFlag
case object MandatedRequiresFlag extends RequiresAccessFlag

object RequiresAccessFlag extends AccessFlag[RequiresAccessFlag] {

  val ACC_TRANSITIVE: Int = 0x0020
  val ACC_STATIC_PHASE: Int = 0x0040
  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_MANDATED: Int = 0x8000

  override val FlagMap: Map[Int, RequiresAccessFlag] = {
    val builder = Map.newBuilder[Int, RequiresAccessFlag]
    builder += ACC_TRANSITIVE -> TransitiveRequiresFlag
    builder += ACC_STATIC_PHASE -> StaticPhaseRequiresFlag
    builder += ACC_SYNTHETIC -> SyntheticRequiresFlag
    builder += ACC_MANDATED -> MandatedRequiresFlag
    builder.result()
  }

  override val Names: Map[RequiresAccessFlag, String] = {
    val builder = Map.newBuilder[RequiresAccessFlag, String]
    builder += TransitiveRequiresFlag -> "ACC_TRANSITIVE"
    builder += StaticPhaseRequiresFlag -> "ACC_STATIC_PHASE"
    builder += SyntheticRequiresFlag -> "ACC_SYNTHETIC"
    builder += MandatedRequiresFlag -> "ACC_MANDATED"
    builder.result()
  }
}
