package io.github.emilyydev.classreader

import java.{lang => jl}

package object bytecode {

  val NewLine: String = System.lineSeparator()

  implicit class ByteToUnsignedInt(private val value: Byte) extends AnyVal {
    def toUnsignedInt: Int = jl.Byte.toUnsignedInt(value)
  }

  implicit class ShortToUnsignedInt(private val value: Short) extends AnyVal {
    def toUnsignedInt: Int = jl.Short.toUnsignedInt(value)
  }
}
