package cubic.complex

import org.hammerlab.math.polynomial.result.Stats

/**
 * [[Cubic.doubleComplex]] is quite slow using [[BigDecimal]]s in JS, so most of hte property-checks are way scaled down
 * here vs the [[Double]] and JVM-based counterparts.
 */
class BigDecimalJSTest
  extends BigDecimalTest {

  override val casePrintInterval: Int = 10

  val M = 2
  val sweep =
    expected(
      Stats(
        85,
        3.58e-16,
        3.11e-16,
        9.93e-16
      ),
      Stats(
        85,
        2.33e-16,
        1.95e-16,
        6.66e-16
      )
    )

  val iterationsPerRootShape = 15
  val random =
    expected(
      Stats(
        60,
        1.77e-12,
        4e-12,
        1.71e-11
      ),
      Stats(
        60,
        2.01e-12,
        3.69e-12,
        1.05e-11
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        60,
        3.37e-12,
        9.06e-12,
        4.64e-11
      ),
      Stats(
        60,
        1.92e-12,
        3.61e-12,
        1.16e-11
      )
    )
}
