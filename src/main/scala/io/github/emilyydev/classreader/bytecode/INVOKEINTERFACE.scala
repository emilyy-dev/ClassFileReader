package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object INVOKEINTERFACE extends Instruction {

  override val mnemonic: String = "invokeinterface"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val methodRefConstantIndex = codeBuffer.getShort().toUnsignedInt
    val methodRefConstant = constantPool.render(methodRefConstantIndex)
    val count = codeBuffer.get().toUnsignedInt
    val zero = codeBuffer.get()
    writer.write(s"$whitespacePadded #$methodRefConstantIndex $count $zero  // $methodRefConstant")
  }
}
