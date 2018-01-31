package org.hammerlab.math.format

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
  val default = apply('e' + _.toString)

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

  object instances {
    implicit val default = Exponent.default
    implicit val cased = Exponent.cased
  }
}
