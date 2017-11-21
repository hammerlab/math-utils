package org.hammerlab.math

import spire.implicits._
import spire.math._

trait Interpolate {
  def interpolate[N: Integral](start: N, end: N, delta: Rational): Rational =
    Rational(start.toSafeLong) + delta * Rational((end - start).toSafeLong)

  def interpolate[N: Numeric](start: N, end: N, delta: Double): Double =
    start.toDouble() + delta * (end - start).toDouble()
}
