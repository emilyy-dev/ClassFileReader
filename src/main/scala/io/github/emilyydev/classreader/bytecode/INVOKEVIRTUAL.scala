package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object INVOKEVIRTUAL extends Instruction {

  override val mnemonic: String = "invokevirtual"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val methodRefConstantIndex = codeBuffer.getShort().toUnsignedInt
    writer.write(
      s"$whitespacePadded #$methodRefConstantIndex  // ${constantPool.render(methodRefConstantIndex)}"
    )
  }
}
