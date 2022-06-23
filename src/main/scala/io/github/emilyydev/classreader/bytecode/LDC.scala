package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object LDC extends Instruction {

  override val mnemonic: String = "ldc"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val constantConstantIndex = codeBuffer.get().toUnsignedInt
    writer.write(
      s"$whitespacePadded #$constantConstantIndex  // ${constantPool.render(constantConstantIndex)}"
    )
  }
}
