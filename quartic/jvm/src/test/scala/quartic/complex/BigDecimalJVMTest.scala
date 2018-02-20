package quartic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJVMTest
  extends BigDecimalTest {

  override val casePrintInterval: Int = 50

  val M = 2
  val sweep =
    expected(
      Stats(
        275,
        3.66e-16,
        4.17e-16,
        2.66e-15
      ),
      Stats(
        275,
        2.65e-16,
        3.04e-16,
        1.52e-15
      )
    )


  val iterationsPerRootShape = 20
  val random =
    expected(
      Stats(
        180,
        8.29e-13,
        2.72e-12,
        1.83e-11
      ),
      Stats(
        180,
        1.18e-12,
        3.85e-12,
        2.54e-11
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        180,
        2.87e-10,
        2.78e-9,
        3.23e-8
      ),
      Stats(
        180,
        6.99e-11,
        6.78e-10,
        8.22e-9
      )
    )
}
