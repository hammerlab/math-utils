package org.hammerlab.math

import cats.Show, Show.show

package object format {
  /**
   * Simple printer for small exponents/superscripts
   */
  val showSuperscript: Show[Int] =
    show {
      case 1 ⇒ ""
      case 2 ⇒ "²"
      case 3 ⇒ "³"
      case 4 ⇒ "⁴"
      case 5 ⇒ "⁵"
      case 6 ⇒ "⁶"
      case 7 ⇒ "⁷"
      case 8 ⇒ "⁸"
      case 9 ⇒ "⁹"
      case n ⇒
        throw new IllegalArgumentException(
          s"Unexpected superscript: $n"
        )
    }
}
