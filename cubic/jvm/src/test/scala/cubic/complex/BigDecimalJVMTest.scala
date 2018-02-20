package cubic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJVMTest
  extends BigDecimalTest {

  ε = 1e-15

  override val casePrintInterval: Int = 50

  val M = 5
  val sweep =
    expected(
      Stats(
        891,
        9.455e-16,
        6.085e-16,
        2.483e-15
      ),
      Stats(
        891,
        3.555e-16,
        2.706e-16,
        1.333e-15
      )
    )

  val iterationsPerRootShape = 200
  val random =
    expected(
      Stats(
        800,
        2.163e-16,
        1.922e-16,
        1.007e-15
      ),
      Stats(
        800,
        8.671e-16,
        2.727e-15,
        4.358e-14
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        800,
        4.887e-16,
        6.108e-16,
        6.075e-15
      ),
      Stats(
        800,
        6.757e-16,
        1.282e-15,
        1.461e-14
      )
    )
}
