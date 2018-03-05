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

  sweepTests(
    (3) →
      Stats(
        15,
        0,
        0,
        0
      ),
    (2,1) →
      Stats(
        60,
        9.06e-17,
        1.37e-16,
        4.44e-16
      ),
    (1,1,1) →
      Stats(
        30,
        6.38e-17,
        1.09e-16,
        3.33e-16
      ),
    (1||1) →
      Stats(
        150,
        1.14e-16,
        1.21e-16,
        4.09e-16
      )
  )

  val iterationsPerRootShape = 15
  gaussians(
    (3) →
      Stats(
        45,
        5.76e-12,
        3.95e-12,
        1.15e-11
      ),
    (2,1) →
      Stats(
        45,
        1.19e-16,
        1.23e-16,
        4.56e-16
      ),
    (1,1,1) →
      Stats(
        45,
        1.16e-16,
        1.01e-16,
        3.48e-16
      ),
    (1||1) →
      Stats(
        45,
        1.33e-16,
        1.14e-16,
        4.11e-16
      )
  )

  logNormals(
    (3) →
      Stats(
        45,
        5.7e-12,
        3.1e-12,
        1.2e-11
      ),
    (2,1) →
      Stats(
        45,
        1.24e-16,
        1.22e-16,
        4e-16
      ),
    (1,1,1) →
      Stats(
        45,
        1.04e-16,
        9.69e-17,
        4.19e-16
      ),
    (1||1) →
      Stats(
        45,
        1.38e-16,
        1.14e-16,
        4.11e-16
      )
  )
}
