package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object IF_ICMPEQ extends Instruction {

  override val mnemonic: String = "if_icmpeq"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val ifPosition = codeBuffer.position() - 1
    writer.write(s"$whitespacePadded ${ifPosition + codeBuffer.getShort()}")
  }
}
