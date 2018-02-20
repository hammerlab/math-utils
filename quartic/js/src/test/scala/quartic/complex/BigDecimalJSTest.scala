package quartic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJSTest
  extends BigDecimalTest {

  override val casePrintInterval: Int = 1

  val M = 1
  val sweep =
    expected(
      Stats(
        39,
        1.3e-16,
        1.89e-16,
        5.56e-16
      ),
      Stats(
        39,
        1.22e-16,
        1.77e-16,
        5.56e-16
      )
    )

  val iterationsPerRootShape = 2
  val random =
    expected(
      Stats(
        18,
        1.89e-13,
        5.99e-13,
        2.53e-12
      ),
      Stats(
        18,
        6.61e-13,
        1.87e-12,
        7.03e-12
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        18,
        4.61e-10,
        1.89e-9,
        8.25e-9
      ),
      Stats(
        18,
        8.25e-10,
        3.4e-9,
        1.48e-8
      )
    )
}
