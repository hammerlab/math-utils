package hammerlab

import org.hammerlab.math.all
import org.hammerlab.{ math ⇒ ohm }

/**
 * Expose various features for importing via:
 *
 * {{{
 * import hammerlab.math._
 * }}}
 */
object math
  extends all {
  type HypergeometricDistribution = ohm.HypergeometricDistribution
  val  HypergeometricDistribution = ohm.HypergeometricDistribution
}
