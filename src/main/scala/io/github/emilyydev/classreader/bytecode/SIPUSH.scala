package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object SIPUSH extends Instruction {

  override val mnemonic: String = "sipush"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit =
    writer.write(s"$whitespacePadded ${codeBuffer.getShort()}")
}
