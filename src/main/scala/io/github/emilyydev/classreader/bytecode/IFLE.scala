package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object IFLE extends Instruction {

  override val mnemonic: String = "ifle"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val ifPosition = codeBuffer.position() - 1
    writer.write(s"$whitespacePadded ${ifPosition + codeBuffer.getShort()}")
  }
}
