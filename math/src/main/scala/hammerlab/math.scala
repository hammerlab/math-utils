package hammerlab

import org.hammerlab.math.{ Steps, all }
import org.hammerlab.{ math ⇒ ohm }

/**
 * Expose various features for importing via:
 *
 * {{{
 * import hammerlab.math._
 * }}}
 */
object math
  extends all
    with Steps {
  type HypergeometricDistribution = ohm.HypergeometricDistribution
  val  HypergeometricDistribution = ohm.HypergeometricDistribution

  type RoundNumbers[I] = ohm.RoundNumbers[I]
  val  RoundNumbers = ohm.RoundNumbers

  val Steps = ohm.Steps
}
