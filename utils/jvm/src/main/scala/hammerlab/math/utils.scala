package hammerlab.math

import org.hammerlab.math.all

/**
 * Expose various features for importing via:
 *
 * {{{
 * import hammerlab.math._
 * }}}
 */
object utils
  extends all {
  import org.hammerlab.{ math ⇒ ohm }
  type HypergeometricDistribution = ohm.HypergeometricDistribution
  val  HypergeometricDistribution = ohm.HypergeometricDistribution
}
