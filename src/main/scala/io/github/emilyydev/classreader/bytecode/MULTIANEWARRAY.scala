package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object MULTIANEWARRAY extends Instruction {

  override val mnemonic: String = "multianewarray"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val classRefConstantIndex = codeBuffer.getShort().toUnsignedInt
    val classRefConstant = constantPool.render(classRefConstantIndex)
    val dimensions = codeBuffer.get().toUnsignedInt
    writer.write(s"$whitespacePadded #$classRefConstantIndex $dimensions  // $classRefConstant")
  }
}
