package io.github.emilyydev

import scala.annotation.tailrec

package object classreader {

  @tailrec
  def countDigits(n: Int, acc: Int = 0): Int = n / 10 match {
    case 0 => acc + 1
    case m => countDigits(m, acc + 1)
  }
}
