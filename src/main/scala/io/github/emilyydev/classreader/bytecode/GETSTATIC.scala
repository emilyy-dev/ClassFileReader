package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object GETSTATIC extends Instruction {

  override val mnemonic: String = "getstatic"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val fieldRefConstantIndex = codeBuffer.getShort().toUnsignedInt
    writer.write(
      s"$whitespacePadded #$fieldRefConstantIndex  // ${constantPool.render(fieldRefConstantIndex)}"
    )
  }
}
