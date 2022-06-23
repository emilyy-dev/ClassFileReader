package io.github.emilyydev.classreader.accessflag

sealed trait InnerClassAccessFlag

case object PublicInnerClassFlag extends InnerClassAccessFlag
case object PrivateInnerClassFlag extends InnerClassAccessFlag
case object ProtectedInnerClassFlag extends InnerClassAccessFlag
case object StaticInnerClassFlag extends InnerClassAccessFlag
case object FinalInnerClassFlag extends InnerClassAccessFlag
case object InterfaceInnerClassFlag extends InnerClassAccessFlag
case object AbstractInnerClassFlag extends InnerClassAccessFlag
case object SyntheticInnerClassFlag extends InnerClassAccessFlag
case object AnnotationInnerClassFlag extends InnerClassAccessFlag
case object EnumInnerClassFlag extends InnerClassAccessFlag

object InnerClassAccessFlag extends AccessFlag[InnerClassAccessFlag] {

  val ACC_PUBLIC: Int = 0x0001
  val ACC_PRIVATE: Int = 0x0002
  val ACC_PROTECTED: Int = 0x0004
  val ACC_STATIC: Int = 0x0008
  val ACC_FINAL: Int = 0x0010
  val ACC_INTERFACE: Int = 0x0200
  val ACC_ABSTRACT: Int = 0x0400
  val ACC_SYNTHETIC: Int = 0x1000
  val ACC_ANNOTATION: Int = 0x2000
  val ACC_ENUM: Int = 0x4000

  override val FlagMap: Map[Int, InnerClassAccessFlag] = {
    val builder = Map.newBuilder[Int, InnerClassAccessFlag]
    builder += ACC_PUBLIC -> PublicInnerClassFlag
    builder += ACC_PRIVATE -> PrivateInnerClassFlag
    builder += ACC_PROTECTED -> ProtectedInnerClassFlag
    builder += ACC_STATIC -> StaticInnerClassFlag
    builder += ACC_FINAL -> FinalInnerClassFlag
    builder += ACC_INTERFACE -> InterfaceInnerClassFlag
    builder += ACC_ABSTRACT -> AbstractInnerClassFlag
    builder += ACC_SYNTHETIC -> SyntheticInnerClassFlag
    builder += ACC_ANNOTATION -> AnnotationInnerClassFlag
    builder += ACC_ENUM -> EnumInnerClassFlag
    builder.result()
  }

  override val Names: Map[InnerClassAccessFlag, String] = {
    val builder = Map.newBuilder[InnerClassAccessFlag, String]
    builder += PublicInnerClassFlag -> "ACC_PUBLIC"
    builder += PrivateInnerClassFlag -> "ACC_PRIVATE"
    builder += ProtectedInnerClassFlag -> "ACC_PROTECTED"
    builder += StaticInnerClassFlag -> "ACC_STATIC"
    builder += FinalInnerClassFlag -> "ACC_FINAL"
    builder += InterfaceInnerClassFlag -> "ACC_INTERFACE"
    builder += AbstractInnerClassFlag -> "ACC_ABSTRACT"
    builder += SyntheticInnerClassFlag -> "ACC_SYNTHETIC"
    builder += AnnotationInnerClassFlag -> "ACC_ANNOTATION"
    builder += EnumInnerClassFlag -> "ACC_ENUM"
    builder.result()
  }
}
