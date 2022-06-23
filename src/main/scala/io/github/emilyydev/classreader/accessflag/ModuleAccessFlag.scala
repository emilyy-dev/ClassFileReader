package io.github.emilyydev.classreader.accessflag

sealed trait ModuleAccessFlag

case object OpenModuleFlag extends ModuleAccessFlag
case object SyntheticModuleFlag extends ModuleAccessFlag
case object MandatedModuleFlag extends ModuleAccessFlag

object ModuleAccessFlag extends AccessFlag[ModuleAccessFlag] {

  val ACC_OPEN: Int = 0x0020
  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_MANDATED: Int = 0x8000

  override val FlagMap: Map[Int, ModuleAccessFlag] = {
    val builder = Map.newBuilder[Int, ModuleAccessFlag]
    builder += ACC_OPEN -> OpenModuleFlag
    builder += ACC_SYNTHETIC -> SyntheticModuleFlag
    builder += ACC_MANDATED -> MandatedModuleFlag
    builder.result()
  }

  override val Names: Map[ModuleAccessFlag, String] = {
    val builder = Map.newBuilder[ModuleAccessFlag, String]
    builder += OpenModuleFlag -> "ACC_OPEN"
    builder += SyntheticModuleFlag -> "ACC_SYNTHETIC"
    builder += MandatedModuleFlag -> "ACC_MANDATED"
    builder.result()
  }
}
