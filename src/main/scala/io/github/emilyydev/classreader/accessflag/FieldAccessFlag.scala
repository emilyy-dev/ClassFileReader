package io.github.emilyydev.classreader.accessflag

sealed trait FieldAccessFlag

case object PublicFieldFlag extends FieldAccessFlag
case object PrivateFieldFlag extends FieldAccessFlag
case object ProtectedFieldFlag extends FieldAccessFlag
case object StaticFieldFlag extends FieldAccessFlag
case object FinalFieldFlag extends FieldAccessFlag
case object VolatileFieldFlag extends FieldAccessFlag
case object TransientFieldFlag extends FieldAccessFlag
case object SyntheticFieldFlag extends FieldAccessFlag
case object EnumFieldFlag extends FieldAccessFlag

object FieldAccessFlag extends AccessFlag[FieldAccessFlag] {

  val ACC_PUBLIC: Int = 0x0001
  val ACC_PRIVATE: Int = 0x0002
  val ACC_PROTECTED: Int = 0x0004
  val ACC_STATIC: Int = 0x0008
  val ACC_FINAL: Int = 0x0010
  val ACC_VOLATILE: Int = 0x0040
  val ACC_TRANSIENT: Int = 0x0080
  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_ENUM: Int = 0x4000

  override val FlagMap: Map[Int, FieldAccessFlag] = {
    val builder = Map.newBuilder[Int, FieldAccessFlag]
    builder += ACC_PUBLIC -> PublicFieldFlag
    builder += ACC_PRIVATE -> PrivateFieldFlag
    builder += ACC_PROTECTED -> ProtectedFieldFlag
    builder += ACC_STATIC -> StaticFieldFlag
    builder += ACC_FINAL -> FinalFieldFlag
    builder += ACC_VOLATILE -> VolatileFieldFlag
    builder += ACC_TRANSIENT -> TransientFieldFlag
    builder += ACC_SYNTHETIC -> SyntheticFieldFlag
    builder += ACC_ENUM -> EnumFieldFlag
    builder.result()
  }

  override val Names: Map[FieldAccessFlag, String] = {
    val builder = Map.newBuilder[FieldAccessFlag, String]
    builder += PublicFieldFlag -> "ACC_PUBLIC"
    builder += PrivateFieldFlag -> "ACC_PRIVATE"
    builder += ProtectedFieldFlag -> "ACC_PROTECTED"
    builder += StaticFieldFlag -> "ACC_STATIC"
    builder += FinalFieldFlag -> "ACC_FINAL"
    builder += VolatileFieldFlag -> "ACC_VOLATILE"
    builder += TransientFieldFlag -> "ACC_TRANSIENT"
    builder += SyntheticFieldFlag -> "ACC_SYNTHETIC"
    builder += EnumFieldFlag -> "ACC_ENUM"
    builder.result()
  }
}
