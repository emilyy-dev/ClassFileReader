package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object TABLESWITCH extends Instruction {

  override val mnemonic: String = "tableswitch"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val tableSwitchPosition = codeBuffer.position() - 1
    val default = adjustAlignment(codeBuffer).getInt()
    val low = codeBuffer.getInt()
    val high = codeBuffer.getInt()

    writer.write(mnemonic)
    for (i <- low to high) {
      writer.write(NewLine)
      writer.write(s"    $i -> ${tableSwitchPosition + codeBuffer.getInt()}")
    }

    writer.write(NewLine)
    writer.write(s"    default -> ${tableSwitchPosition + default}")
  }

  private def adjustAlignment(codeBuffer: ByteBuffer): ByteBuffer =
    codeBuffer.position(codeBuffer.position() + (4 - codeBuffer.position() % 4) % 4)
}
