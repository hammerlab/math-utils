package quartic.complex

import hammerlab.scalajs._
import org.hammerlab.math.polynomial.TestCase
import org.hammerlab.math.polynomial.result.Stats
import spire.implicits._

class DoubleTest
  extends QuarticTest[Double] {

  /**
   * This controls the sensitivity of marking depressed-quartic coefficients as "zero"
   *
   * The [[TestCase]]s with the worst numerical-imprecision artifacts tend to be polynomials of the form (x-r)(x-r-ε);
   * the constant term of the depressed quartic is O(ε⁴), and comes from subtracting terms that are O(r).
   */
  ε = 1e-8

  override val casePrintInterval = 2000

  val M = 7

  val sweep =
    expected(
      Stats(
        21225,
        2.51e-9,
        1.31e-8,
        3.99e-7
      ),
      Stats(
        21225,
        7.82e-10,
        4.03e-9,
        9.32e-8
      )
    )

  val iterationsPerRootShape: Int = 2000
  val random =
    expected(
      Stats(
        18000,
        7.37e-7,
        3.09e-6,
        9.15e-5
      ),
      Stats(
        18000,
        1.36e-6,
        1.42e-5,
        0.00148
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        18000,
        js{ 1.43e-6 }{ 1.43e-6 },
        js{ 6.97e-6 }{ 7.07e-6 },
        js{ 1.99e-4 }{ 2.25e-4 }
      ),
      Stats(
        18000,
        js{ 1.00e-6 }{ 9.84e-7 },
        js{ 4.25e-6 }{ 3.68e-6 },
        js{ 3.10e-4 }{ 1.55e-4 }
      )
    )
}
