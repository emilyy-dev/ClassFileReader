package io.github.emilyydev.classreader.accessflag

sealed trait MethodAccessFlag

case object PublicMethodFlag extends MethodAccessFlag
case object PrivateMethodFlag extends MethodAccessFlag
case object ProtectedMethodFlag extends MethodAccessFlag
case object StaticMethodFlag extends MethodAccessFlag
case object FinalMethodFlag extends MethodAccessFlag
case object SynchronizedMethodFlag extends MethodAccessFlag
case object BridgeMethodFlag extends MethodAccessFlag
case object VarargsMethodFlag extends MethodAccessFlag
case object NativeMethodFlag extends MethodAccessFlag
case object AbstractMethodFlag extends MethodAccessFlag
case object StrictMethodFlag extends MethodAccessFlag
case object SyntheticMethodFlag extends MethodAccessFlag

object MethodAccessFlag extends AccessFlag[MethodAccessFlag] {

  val ACC_PUBLIC: Int = 0x0001
  val ACC_PRIVATE: Int = 0x0002
  val ACC_PROTECTED: Int = 0x0004
  val ACC_STATIC: Int = 0x0008
  val ACC_FINAL: Int = 0x0010
  val ACC_SYNCHRONIZED: Int = 0x0020
  val ACC_BRIDGE: Int = 0x0040
  val ACC_VARARGS: Int = 0x0080
  val ACC_NATIVE: Int = 0x0100
  val ACC_ABSTRACT: Int = 0x0400
  val ACC_STRICT: Int = 0x0800
  val ACC_SYNTHETIC: Int = 0x1000

  override val FlagMap: Map[Int, MethodAccessFlag] = {
    val builder = Map.newBuilder[Int, MethodAccessFlag]
    builder += ACC_PUBLIC -> PublicMethodFlag
    builder += ACC_PRIVATE -> PrivateMethodFlag
    builder += ACC_PROTECTED -> ProtectedMethodFlag
    builder += ACC_STATIC -> StaticMethodFlag
    builder += ACC_FINAL -> FinalMethodFlag
    builder += ACC_SYNCHRONIZED -> SynchronizedMethodFlag
    builder += ACC_BRIDGE -> BridgeMethodFlag
    builder += ACC_VARARGS -> VarargsMethodFlag
    builder += ACC_NATIVE -> NativeMethodFlag
    builder += ACC_ABSTRACT -> AbstractMethodFlag
    builder += ACC_STRICT -> StrictMethodFlag
    builder += ACC_SYNTHETIC -> SyntheticMethodFlag
    builder.result()
  }

  override val Names: Map[MethodAccessFlag, String] = {
    val builder = Map.newBuilder[MethodAccessFlag, String]
    builder += PublicMethodFlag -> "ACC_PUBLIC"
    builder += PrivateMethodFlag -> "ACC_PRIVATE"
    builder += ProtectedMethodFlag -> "ACC_PROTECTED"
    builder += StaticMethodFlag -> "ACC_STATIC"
    builder += FinalMethodFlag -> "ACC_FINAL"
    builder += SynchronizedMethodFlag -> "ACC_SYNCHRONIZED"
    builder += BridgeMethodFlag -> "ACC_BRIDGE"
    builder += VarargsMethodFlag -> "ACC_VARARGS"
    builder += NativeMethodFlag -> "ACC_NATIVE"
    builder += AbstractMethodFlag -> "ACC_ABSTRACT"
    builder += StrictMethodFlag -> "ACC_STRICT"
    builder += SyntheticMethodFlag -> "ACC_SYNTHETIC"
    builder.result()
  }
}
