package io.github.emilyydev.classreader.accessflag

sealed trait AccessFlag

case object PublicAccessFlag extends AccessFlag
case object PrivateAccessFlag extends AccessFlag
case object ProtectedAccessFlag extends AccessFlag
case object StaticAccessFlag extends AccessFlag
case object FinalAccessFlag extends AccessFlag
case object OpenAccessFlag extends AccessFlag
case object SuperAccessFlag extends AccessFlag
case object TransitiveAccessFlag extends AccessFlag
case object SynchronizedAccessFlag extends AccessFlag
case object BridgeAccessFlag extends AccessFlag
case object VolatileAccessFlag extends AccessFlag
case object StaticPhaseAccessFlag extends AccessFlag
case object VarargsAccessFlag extends AccessFlag
case object TransientAccessFlag extends AccessFlag
case object NativeAccessFlag extends AccessFlag
case object InterfaceAccessFlag extends AccessFlag
case object AbstractAccessFlag extends AccessFlag
case object StrictAccessFlag extends AccessFlag
case object SyntheticAccessFlag extends AccessFlag
case object AnnotationAccessFlag extends AccessFlag
case object EnumAccessFlag extends AccessFlag
case object ModuleAccessFlag extends AccessFlag
case object MandatedAccessFlag extends AccessFlag

object AccessFlag {

  def asSet(isLegalFlag: AccessFlag => Boolean)(flagSet: Int): Set[AccessFlag] =
    Set.from(
      FlagMap
        .filter { case (flagValue, _) => isFlagSet(flagSet, flagValue) }
        .values
        .filter(isLegalFlag)
    )

  def isFlagSet(flagSet: Int, flag: Int): Boolean = (flagSet & flag) == flag

  def setAsNumber(flagSet: Set[AccessFlag]): Int =
    FlagMap
      .filter { case (_, flag) => flagSet.contains(flag) }
      .keys
      .reduce(_ | _)

  val ACC_PUBLIC: Int = 0x0001
  val ACC_PRIVATE: Int = 0x0002
  val ACC_PROTECTED: Int = 0x0004
  val ACC_STATIC: Int = 0x0008
  val ACC_FINAL: Int = 0x0010
  val ACC_OPEN: Int = 0x0020
  val ACC_SUPER: Int = 0x0020
  val ACC_TRANSITIVE: Int = 0x0020
  val ACC_SYNCHRONIZED: Int = 0x0020
  val ACC_BRIDGE: Int = 0x0040
  val ACC_VOLATILE: Int = 0x0040
  val ACC_STATIC_PHASE: Int = 0x0040
  val ACC_VARARGS: Int = 0x0080
  val ACC_TRANSIENT: Int = 0x0080
  val ACC_NATIVE: Int = 0x0100
  val ACC_INTERFACE: Int = 0x0200
  val ACC_ABSTRACT: Int = 0x0400
  val ACC_STRICT: Int = 0x0800
  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_ANNOTATION: Int = 0x2000
  val ACC_ENUM: Int = 0x4000
  val ACC_MODULE: Int = 0x8000
  val ACC_MANDATED: Int = 0x8000

  val FlagMap: Map[Int, AccessFlag] = {
    val builder = Map.newBuilder[Int, AccessFlag]
    builder += ACC_PUBLIC -> PublicAccessFlag
    builder += ACC_PRIVATE -> PrivateAccessFlag
    builder += ACC_PROTECTED -> ProtectedAccessFlag
    builder += ACC_STATIC -> StaticAccessFlag
    builder += ACC_FINAL -> FinalAccessFlag
    builder += ACC_OPEN -> OpenAccessFlag
    builder += ACC_SUPER -> SuperAccessFlag
    builder += ACC_TRANSITIVE -> TransitiveAccessFlag
    builder += ACC_SYNCHRONIZED -> SynchronizedAccessFlag
    builder += ACC_BRIDGE -> BridgeAccessFlag
    builder += ACC_VOLATILE -> VolatileAccessFlag
    builder += ACC_STATIC_PHASE -> StaticPhaseAccessFlag
    builder += ACC_VARARGS -> VarargsAccessFlag
    builder += ACC_TRANSIENT -> TransientAccessFlag
    builder += ACC_NATIVE -> NativeAccessFlag
    builder += ACC_INTERFACE -> InterfaceAccessFlag
    builder += ACC_ABSTRACT -> AbstractAccessFlag
    builder += ACC_STRICT -> StrictAccessFlag
    builder += ACC_SYNTHETIC -> SyntheticAccessFlag
    builder += ACC_ANNOTATION -> AnnotationAccessFlag
    builder += ACC_ENUM -> EnumAccessFlag
    builder += ACC_MODULE -> ModuleAccessFlag
    builder += ACC_MANDATED -> MandatedAccessFlag
    builder.result()
  }

  val Names: Map[AccessFlag, String] = {
    val builder = Map.newBuilder[AccessFlag, String]
    builder += PublicAccessFlag -> "ACC_PUBLIC"
    builder += PrivateAccessFlag -> "ACC_PRIVATE"
    builder += ProtectedAccessFlag -> "ACC_PROTECTED"
    builder += StaticAccessFlag -> "ACC_STATIC"
    builder += FinalAccessFlag -> "ACC_FINAL"
    builder += OpenAccessFlag -> "ACC_OPEN"
    builder += SuperAccessFlag -> "ACC_SUPER"
    builder += TransitiveAccessFlag -> "ACC_TRANSITIVE"
    builder += SynchronizedAccessFlag -> "ACC_SYNCHRONIZED"
    builder += BridgeAccessFlag -> "ACC_BRIDGE"
    builder += VolatileAccessFlag -> "ACC_VOLATILE"
    builder += StaticPhaseAccessFlag -> "ACC_STATIC_PHASE"
    builder += VarargsAccessFlag -> "ACC_VARARGS"
    builder += TransientAccessFlag -> "ACC_TRANSIENT"
    builder += NativeAccessFlag -> "ACC_NATIVE"
    builder += InterfaceAccessFlag -> "ACC_INTERFACE"
    builder += AbstractAccessFlag -> "ACC_ABSTRACT"
    builder += StrictAccessFlag -> "ACC_STRICT"
    builder += SyntheticAccessFlag -> "ACC_SYNTHETIC"
    builder += AnnotationAccessFlag -> "ACC_ANNOTATION"
    builder += EnumAccessFlag -> "ACC_ENUM"
    builder += ModuleAccessFlag -> "ACC_MODULE"
    builder += MandatedAccessFlag -> "ACC_MANDATED"
    builder.result()
  }
}
