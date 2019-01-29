package org.hammerlab.math

import org.hammerlab.{ math ⇒ h }

trait all
  extends Binomial
     with Div
     with Interpolate
     with Min
     with SafeInt.syntax
     with Steps {
  type RoundNumbers[I] = h.RoundNumbers[I]
  val  RoundNumbers = h.RoundNumbers

  type HypergeometricDistribution = h.HypergeometricDistribution
  val  HypergeometricDistribution = h.HypergeometricDistribution

  val Steps = h.Steps

  type CastException = h.CastException
}
