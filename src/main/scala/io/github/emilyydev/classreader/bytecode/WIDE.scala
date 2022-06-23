package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object WIDE extends Instruction {

  override val mnemonic: String = "wide"

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val modifiedOpcode = Instruction.InstructionMap(codeBuffer.get())
    writer.write(s"$whitespacePadded ${modifiedOpcode.mnemonic}")
    writer.write(' ')
    modifiedOpcode match {
      case IINC =>
        val index = codeBuffer.getShort().toUnsignedInt
        val value = codeBuffer.getShort()
        writer.write(s"$index $value")
      case ILOAD |
           FLOAD |
           ALOAD |
           LLOAD |
           DLOAD |
           ISTORE |
           FSTORE |
           ASTORE |
           LSTORE |
           DSTORE |
           RET =>
        val index = codeBuffer.getShort().toUnsignedInt
        writer.write(s"$index")
    }
  }
}
