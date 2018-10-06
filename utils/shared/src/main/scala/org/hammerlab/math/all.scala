package org.hammerlab.math

import org.hammerlab.{ math ⇒ ohm }

trait all
  extends Binomial
     with Div
     with Interpolate
     with Min
     with Steps {
  type RoundNumbers[I] = ohm.RoundNumbers[I]
  val  RoundNumbers = ohm.RoundNumbers

  type HypergeometricDistribution = ohm.HypergeometricDistribution
  val  HypergeometricDistribution = ohm.HypergeometricDistribution

  val Steps = ohm.Steps
}
