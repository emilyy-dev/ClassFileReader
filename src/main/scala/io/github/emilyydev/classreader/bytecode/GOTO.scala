package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object GOTO extends Instruction {

  override val mnemonic: String = "goto"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val gotoPosition = codeBuffer.position() - 1
    writer.write(s"$whitespacePadded ${gotoPosition + codeBuffer.getShort()}")
  }
}
