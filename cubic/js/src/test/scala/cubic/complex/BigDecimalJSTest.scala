package cubic.complex

import org.hammerlab.math.polynomial.result.Stats

/**
 * [[Cubic.doubleComplex]] is quite slow using [[BigDecimal]]s in JS, so most of hte property-checks are way scaled down
 * here vs the [[Double]] and JVM-based counterparts.
 */
class BigDecimalJSTest
  extends BigDecimalTest {

  ε = 1e-15

  override val casePrintInterval: Int = 10

  val M = 3
  val sweep =
    expected(
      Stats(
        231,
        5.547e-16,
        4.139e-16,
        1.49e-15
      ),
      Stats(
        231,
        2.853e-16,
        2.162e-16,
        8.882e-16
      )
    )

  val iterationsPerRootShape = 50
  val random =
    expected(
      Stats(
        200,
        2.057e-16,
        1.9e-16,
        1.001e-15
      ),
      Stats(
        200,
        9.544e-16,
        3.265e-15,
        3.038e-14
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        200,
        4.779e-16,
        6.149e-16,
        3.715e-15
      ),
      Stats(
        200,
        5.717e-16,
        1.188e-15,
        1.287e-14
      )
    )
}
