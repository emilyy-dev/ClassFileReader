package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object INVOKEDYNAMIC extends Instruction {

  override val mnemonic: String = "invokedynamic"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val indyConstantIndex = codeBuffer.getShort().toUnsignedInt
    val indyConstant = constantPool.render(indyConstantIndex)
    val zero = codeBuffer.getShort()
    writer.write(s"$whitespacePadded #$indyConstantIndex $zero  // $indyConstant")
  }
}
