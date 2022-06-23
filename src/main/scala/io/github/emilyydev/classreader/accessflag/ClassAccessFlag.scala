package io.github.emilyydev.classreader.accessflag

sealed trait ClassAccessFlag

case object PublicClassFlag extends ClassAccessFlag
case object FinalClassFlag extends ClassAccessFlag
case object SuperClassFlag extends ClassAccessFlag
case object InterfaceClassFlag extends ClassAccessFlag
case object AbstractClassFlag extends ClassAccessFlag
case object SyntheticClassFlag extends ClassAccessFlag
case object AnnotationClassFlag extends ClassAccessFlag
case object EnumClassFlag extends ClassAccessFlag
case object ModuleClassFlag extends ClassAccessFlag

object ClassAccessFlag extends AccessFlag[ClassAccessFlag] {

  val ACC_PUBLIC: Int = 0x0001
  val ACC_FINAL: Int = 0x0010
  val ACC_SUPER: Int = 0x0020
  val ACC_INTERFACE: Int = 0x0200
  val ACC_ABSTRACT: Int = 0x0400
  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_ANNOTATION: Int = 0x2000
  val ACC_ENUM: Int = 0x4000
  val ACC_MODULE: Int = 0x8000

  override val FlagMap: Map[Int, ClassAccessFlag] = {
    val builder = Map.newBuilder[Int, ClassAccessFlag]
    builder += ACC_PUBLIC -> PublicClassFlag
    builder += ACC_FINAL -> FinalClassFlag
    builder += ACC_SUPER -> SuperClassFlag
    builder += ACC_INTERFACE -> InterfaceClassFlag
    builder += ACC_ABSTRACT -> AbstractClassFlag
    builder += ACC_SYNTHETIC -> SyntheticClassFlag
    builder += ACC_ANNOTATION -> AnnotationClassFlag
    builder += ACC_ENUM -> EnumClassFlag
    builder += ACC_MODULE -> ModuleClassFlag
    builder.result()
  }

  override val Names: Map[ClassAccessFlag, String] = {
    val builder = Map.newBuilder[ClassAccessFlag, String]
    builder += PublicClassFlag -> "ACC_PUBLIC"
    builder += FinalClassFlag -> "ACC_FINAL"
    builder += SuperClassFlag -> "ACC_SUPER"
    builder += InterfaceClassFlag -> "ACC_INTERFACE"
    builder += AbstractClassFlag -> "ACC_ABSTRACT"
    builder += SyntheticClassFlag -> "ACC_SYNTHETIC"
    builder += AnnotationClassFlag -> "ACC_ANNOTATION"
    builder += EnumClassFlag -> "ACC_ENUM"
    builder += ModuleClassFlag -> "ACC_MODULE"
    builder.result()
  }
}
