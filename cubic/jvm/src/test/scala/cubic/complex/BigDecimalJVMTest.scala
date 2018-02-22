package cubic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJVMTest
  extends BigDecimalTest {

  override val casePrintInterval: Int = 50

  val M = 4

  sweepTests(
    3 →
      Stats(
        27,
        0,
        0,
        0
      ),
    (2,1) →
      Stats(
        216,
        1.02e-16,
        1.3e-16,
        4.44e-16
      ),
    (1,1,1) →
      Stats(
        252,
        9.21e-17,
        1.14e-16,
        3.95e-16
      ),
    (1||1) →
      Stats(
        972,
        1.23e-16,
        1.19e-16,
        4.34e-16
      )
  )

  val iterationsPerRootShape = 100

  gaussians(
    (3) →
      Stats(
        300,
        5.1e-12,
        3.66e-12,
        1.27e-11
      ),
    (2,1) →
      Stats(
        300,
        1.19e-16,
        1.19e-16,
        4.56e-16
      ),
    (1,1,1) →
      Stats(
        300,
        1.18e-16,
        9.8e-17,
        3.61e-16
      ),
    (1||1) →
      Stats(
        300,
        1.21e-16,
        1.09e-16,
        4.3e-16
      )
  )

  logNormals(
    (3) →
      Stats(
        300,
        5.14e-12,
        3.7e-12,
        1.43e-11
      ),
    (2,1) →
      Stats(
        300,
        1.26e-16,
        1.22e-16,
        4.74e-16
      ),
    (1,1,1) →
      Stats(
        300,
        1.2e-16,
        9.96e-17,
        4.19e-16
      ),
    (1||1) →
      Stats(
        300,
        1.28e-16,
        1.07e-16,
        4.11e-16
      )
  )
}
