package org.hammerlab

import spire.implicits._
import spire.math._

package object math {
  /**
   * Simple helper for rounding-up integer-division
   */
  def ceil[N: Integral](numerator: N, denominator: N): N = {
    val numeric = implicitly[Integral[N]]
    import numeric._
    fromDouble(
      scala.math.ceil(
        toDouble(numerator) / toDouble(denominator)
      )
    )
  }

  def interpolate[N: Integral](start: N, end: N, delta: Rational): Rational =
    Rational(start.toSafeLong) + delta * Rational((end - start).toSafeLong)

  def interpolate[N: Numeric](start: N, end: N, delta: Double): Double =
    start.toDouble() + delta * (end - start).toDouble()

  def min(a: Int, b: Long): Int =
    if (a.toLong <= b)
      a
    else
      b.toInt

  def min(a: Long, b: Int): Int =
    if (a <= b.toLong)
      a.toInt
    else
      b
}
