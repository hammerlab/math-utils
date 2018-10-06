package org.hammerlab.math

import spire.math._
import spire.implicits._

trait Div {
  /**
   * Simple helper for rounding-up integer-division
   */
  def div[N: Integral](numerator: N, denominator: N): N = {
    val numeric = implicitly[Integral[N]]
    import numeric._
    fromDouble(
      scala.math.ceil(
        toDouble(numerator) / toDouble(denominator)
      )
    )
  }

  /**
   * Deprecated alternate name
   */
  def ceil[N: Integral](numerator: N, denominator: N): N = div(numerator, denominator)

  @inline implicit def toDivOps[N](n: N): Div.Ops[N] = Div.Ops(n)
}

object Div {
  implicit class Ops[N](val l: N) extends AnyVal {
    def /↑(r: N)(implicit n: Integral[N]): N = n.quot(l + r - 1, r)
  }
}
