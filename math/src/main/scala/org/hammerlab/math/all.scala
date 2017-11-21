package org.hammerlab.math

import org.hammerlab.{ math ⇒ ohm }

trait types {
  type HypergeometricDistribution = ohm.HypergeometricDistribution
  val  HypergeometricDistribution = ohm.HypergeometricDistribution

  type RoundNumbers[I] = ohm.RoundNumbers[I]
  val  RoundNumbers = ohm.RoundNumbers

  val Steps = ohm.Steps
}

trait all
  extends Div
    with Format
    with Interpolate
    with Min
    with Steps
