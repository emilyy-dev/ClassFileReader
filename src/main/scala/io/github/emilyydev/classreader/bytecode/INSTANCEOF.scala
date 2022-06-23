package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object INSTANCEOF extends Instruction {

  override val mnemonic: String = "instanceof"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val classRefConstantIndex = codeBuffer.getShort().toUnsignedInt
    writer.write(
      s"$whitespacePadded #$classRefConstantIndex  // ${constantPool.render(classRefConstantIndex)}"
    )
  }
}