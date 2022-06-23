package io.github.emilyydev.classreader.bytecode

import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.Writer
import java.nio.ByteBuffer

case object NEWARRAY extends Instruction {

  override val mnemonic: String = "newarray"

  val T_BOOLEAN: Int = 4
  val T_CHAR: Int = 5
  val T_FLOAT: Int = 6
  val T_DOUBLE: Int = 7
  val T_BYTE: Int = 8
  val T_SHORT: Int = 9
  val T_INT: Int = 10
  val T_LONG: Int = 11

  override def render(codeBuffer: ByteBuffer, constantPool: ConstantPool, writer: Writer): Unit = {
    val arrayType = codeBuffer.get().toUnsignedInt
    writer.write(s"$whitespacePadded $arrayType (${typeName(arrayType)})")
  }

  private def typeName(arrayType: Int): String = arrayType match {
    case T_BOOLEAN => "Z"
    case T_CHAR => "C"
    case T_FLOAT => "F"
    case T_DOUBLE => "D"
    case T_BYTE => "B"
    case T_SHORT => "S"
    case T_INT => "I"
    case T_LONG => "J"
    case _ => "?"
  }
}
