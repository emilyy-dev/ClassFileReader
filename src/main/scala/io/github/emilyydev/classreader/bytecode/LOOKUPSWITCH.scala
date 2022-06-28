package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object LOOKUPSWITCH extends Instruction {

  override val mnemonic: String = "lookupswitch"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val lookupSwitchPosition = codeBuffer.position() - 1
    val default = adjustAlignment(codeBuffer).getInt()
    val pairCount = codeBuffer.getInt()
    val matchOffsetPairs = for {
      _ <- 0 until pairCount
    } yield (codeBuffer.getInt(), codeBuffer.getInt())

    writer.write(mnemonic)
    for ((key, offset) <- matchOffsetPairs) {
      writer.write(NewLine)
      writer.write(s"    $key -> ${lookupSwitchPosition + offset}")
    }

    writer.write(NewLine)
    writer.write(s"    default -> ${lookupSwitchPosition + default}")
  }

  private def adjustAlignment(codeBuffer: ByteBuffer): ByteBuffer =
    codeBuffer.position(codeBuffer.position() + (4 - codeBuffer.position() % 4) % 4)
}
