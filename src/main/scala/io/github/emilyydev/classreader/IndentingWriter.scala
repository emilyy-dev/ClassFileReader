package io.github.emilyydev.classreader

import java.io.Writer

class IndentingWriter(private val out: Writer) extends Writer with Indentable {

  private var wroteNewLine: Boolean = false
  private var indentationString = ""
  private def incrementIndentation(delta: Int): this.type = {
    val current = indentationString.length
    indentationString = " ".repeat(math.max(0, current + delta))
    this
  }

  override def increaseIndentation(): this.type = incrementIndentation(2)
  override def decreaseIndentation(): this.type = incrementIndentation(-2)

  override def write(buff: Array[Char], off: Int, len: Int): Unit = {
    var from = off
    for (i <- off until off + len) {
      if (buff(i) == '\n') {
        out.write(buff, from, i + 1 - from)
        from = i + 1
        wroteNewLine = true
      } else if (wroteNewLine) {
        out.write(indentationString)
        wroteNewLine = false
      }
    }

    out.write(buff, from, len - from)
  }

  override def flush(): Unit = out.flush()
  override def close(): Unit = out.close()
}
