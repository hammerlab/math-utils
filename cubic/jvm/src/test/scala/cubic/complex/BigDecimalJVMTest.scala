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
        1.339e-12,
        3.467e-12,
        2.154e-11
      ),
      Stats(
        800,
        1.681e-12,
        3.482e-12,
        1.259e-11
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        800,
        2.746e-12,
        9.409e-12,
        1.442e-10
      ),
      Stats(
        800,
        1.692e-12,
        3.448e-12,
        1.218e-11
      )
    )
}
