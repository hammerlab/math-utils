package org.hammerlab.math

import org.hammerlab.{ math ⇒ ohm }

trait all
  extends HasBinomial
     with Div
     with Interpolate
     with Min
     with Steps {
  type RoundNumbers[I] = ohm.RoundNumbers[I]
  val  RoundNumbers = ohm.RoundNumbers

  val Steps = ohm.Steps
}
