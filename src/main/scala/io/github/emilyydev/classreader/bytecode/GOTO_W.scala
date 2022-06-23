package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object GOTO_W extends Instruction {

  override val mnemonic: String = "goto_w"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val gotoWPosition = codeBuffer.position() - 1
    writer.write(s"$whitespacePadded ${gotoWPosition + codeBuffer.getInt()}")
  }
}
