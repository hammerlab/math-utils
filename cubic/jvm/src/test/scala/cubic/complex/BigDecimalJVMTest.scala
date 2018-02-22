package cubic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJVMTest
  extends BigDecimalTest {

  override val casePrintInterval: Int = 50

  val M = 4
  val sweep =
    expected(
      Stats(
        489,
        3.26e-16,
        2.43e-16,
        1.11e-15
      )
    )

  val iterationsPerRootShape = 100
  val random =
    expected(
      Stats(
        400,
        1.86e-12,
        3.65e-12,
        1.26e-11
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        400,
        1.55e-12,
        3.34e-12,
        1.22e-11
      )
    )
}
