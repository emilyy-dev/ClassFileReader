package io.github.emilyydev.classreader

import java.io.Writer

trait Indentable {
  this: Writer =>

  def increaseIndentation(): this.type
  def decreaseIndentation(): this.type

  def newLine(): this.type = {
    write(System.lineSeparator())
    this
  }

  def writeln(str: String): this.type = {
    write(str)
    newLine()
  }
}
