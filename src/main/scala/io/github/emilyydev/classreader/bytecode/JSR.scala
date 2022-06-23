package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object JSR extends Instruction {

  override val mnemonic: String = "jsr"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val jsrPosition = codeBuffer.position() - 1
    writer.write(s"$whitespacePadded ${jsrPosition + codeBuffer.getShort()}")
  }
}
