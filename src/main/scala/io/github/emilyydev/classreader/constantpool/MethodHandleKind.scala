package io.github.emilyydev.classreader.constantpool

sealed trait MethodHandleKind

case object GetFieldKind extends MethodHandleKind
case object GetStaticKind extends MethodHandleKind
case object PutFieldKind extends MethodHandleKind
case object PutStaticKind extends MethodHandleKind
case object InvokeVirtualKind extends MethodHandleKind
case object InvokeStaticKind extends MethodHandleKind
case object InvokeSpecialKind extends MethodHandleKind
case object NewInvokeSpecialKind extends MethodHandleKind
case object InvokeInterfaceKind extends MethodHandleKind

object MethodHandleKind {

  val REF_getField: Int = 1
  val REF_getStatic: Int = 2
  val REF_putField: Int = 3
  val REF_putStatic: Int = 4
  val REF_invokeVirtual: Int = 5
  val REF_invokeStatic: Int = 6
  val REF_invokeSpecial: Int = 7
  val REF_newInvokeSpecial: Int = 8
  val REF_invokeInterface: Int = 9

  val KindMap: Map[Int, MethodHandleKind] = {
    val builder = Map.newBuilder[Int, MethodHandleKind]
    builder += REF_getField -> GetFieldKind
    builder += REF_getStatic -> GetStaticKind
    builder += REF_putField -> PutFieldKind
    builder += REF_putStatic -> PutStaticKind
    builder += REF_invokeVirtual -> InvokeVirtualKind
    builder += REF_invokeStatic -> InvokeStaticKind
    builder += REF_invokeSpecial -> InvokeSpecialKind
    builder += REF_newInvokeSpecial -> NewInvokeSpecialKind
    builder += REF_invokeInterface -> InvokeInterfaceKind
    builder.result()
  }

  val Names: Map[MethodHandleKind, String] = {
    val builder = Map.newBuilder[MethodHandleKind, String]
    builder += GetFieldKind -> "REF_getField"
    builder += GetStaticKind -> "REF_getStatic"
    builder += PutFieldKind -> "REF_putField"
    builder += PutStaticKind -> "REF_putStatic"
    builder += InvokeVirtualKind -> "REF_invokeVirtual"
    builder += InvokeStaticKind -> "REF_invokeStatic"
    builder += InvokeSpecialKind -> "REF_invokeSpecial"
    builder += NewInvokeSpecialKind -> "REF_newInvokeSpecial"
    builder += InvokeInterfaceKind -> "REF_invokeInterface"
    builder.result()
  }
}
