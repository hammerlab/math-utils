package org.hammerlab.math.format

import cats.Show
import cats.Show.show

/**
 * Configurable strategy for rendering the exponent-portion of a number in scientific-notation
 */
trait Exponent {
  def apply(exp: Int): String
}
object Exponent {
  def apply(fn: Int ⇒ String): Exponent =
    new Exponent {
      override def apply(exp: Int): String = fn(exp)
    }

  /**
   * "Default": "e" plus a signed integer
   */
  implicit val default = apply('e' + _.toString)

  /**
   * "Cased": use the case of the "e" to represent a positive ("E") or negative ("e") exponent (a small optimization for
   * space-saving and uniform-width printing)
   */
  val cased  =
    apply(
      exp ⇒
        if (exp > 0)
          s"E$exp"
        else
          s"e$exp"
    )

  trait instances {
    implicit val cased = Exponent.cased
  }

  trait format {
    /**
     * Simple printer for small exponents/superscripts
     */
    implicit val showSuperscript: Show[Int] =
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
}
