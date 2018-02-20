package cubic.complex

import org.hammerlab.math.polynomial.result.Stats

/**
 * [[Cubic.doubleComplex]] is quite slow using [[BigDecimal]]s in JS, so most of hte property-checks are way scaled down
 * here vs the [[Double]] and JVM-based counterparts.
 */
class BigDecimalJSTest
  extends BigDecimalTest {

  override val casePrintInterval: Int = 10

  val M = 3
  val sweep =
    expected(
      Stats(
        231,
        5.55e-16,
        4.14e-16,
        1.49e-15
      ),
      Stats(
        231,
        2.85e-16,
        2.16e-16,
        8.88e-16
      )
    )

  val iterationsPerRootShape = 50
  val random =
    expected(
      Stats(
        200,
        1.41e-12,
        3.37e-12,
        1.71e-11
      ),
      Stats(
        200,
        1.93e-12,
        3.75e-12,
        1.26e-11
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        200,
        2.59e-12,
        7.33e-12,
        4.64e-11
      ),
      Stats(
        200,
        1.78e-12,
        3.55e-12,
        1.19e-11
      )
    )
}
