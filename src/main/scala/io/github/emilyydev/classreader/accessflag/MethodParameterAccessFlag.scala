package io.github.emilyydev.classreader.accessflag

sealed trait MethodParameterAccessFlag

case object FinalMethodParameterFlag extends MethodParameterAccessFlag
case object SyntheticMethodParameterFlag extends MethodParameterAccessFlag
case object MandatedMethodParameterFlag extends MethodParameterAccessFlag

object MethodParameterAccessFlag extends AccessFlag[MethodParameterAccessFlag] {

  val ACC_FINAL: Int = 0x0010
  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_MANDATED: Int = 0x8000

  override val FlagMap: Map[Int, MethodParameterAccessFlag] = {
    val builder = Map.newBuilder[Int, MethodParameterAccessFlag]
    builder += ACC_FINAL -> FinalMethodParameterFlag
    builder += ACC_SYNTHETIC -> SyntheticMethodParameterFlag
    builder += ACC_MANDATED -> MandatedMethodParameterFlag
    builder.result()
  }

  override val Names: Map[MethodParameterAccessFlag, String] = {
    val builder = Map.newBuilder[MethodParameterAccessFlag, String]
    builder += FinalMethodParameterFlag -> "ACC_FINAL"
    builder += SyntheticMethodParameterFlag -> "ACC_SYNTHETIC"
    builder += MandatedMethodParameterFlag -> "ACC_MANDATED"
    builder.result()
  }
}
