package io.github.emilyydev.classreader.attribute

import java.io.DataInput

sealed trait TypeAnnotationInfo

object TypeAnnotationInfo {

  val ClassTarget: Int = 0x00
  val MethodTarget: Int = 0x01
  val ExtendsTarget: Int = 0x10
  val BoundTypeClassTarget: Int = 0x11
  val BoundTypeMethodTarget: Int = 0x12
  val FieldTarget: Int = 0x13
  val ReturnTarget: Int = 0x14
  val ReceiverTarget: Int = 0x15
  val ParameterTarget: Int = 0x16
  val ThrowsTarget: Int = 0x17

  def read(in: DataInput): TypeAnnotationInfo = ???
}
