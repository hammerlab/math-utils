package org.hammerlab.math

import spire.math._

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
}
