package cubic.complex

import org.hammerlab.math.polynomial.result.Stats
import spire.implicits._

class DoubleTest
  extends CubicTest[Double] {
  override val casePrintInterval = 1000
  val M = 12
  val iterationsPerRootShape = 2000

  val sweep =
    expected(
      Stats(
        10425,
        3.838e-9,
        3.046e-8,
        8.754e-7
      ),
      Stats(
        10425,
        5.862e-10,
        3.966e-9,
        7.865e-8
      )
    )

  val random =
    expected(
      Stats(
        8000,
        1.84e-7,
        8.427e-6,
        5.266e-4
      ),
      Stats(
        8000,
        5.27e-7,
        2.491e-5,
        0.001632
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        8000,
        2.98e-7,
        2.61e-5,
        0.002335
      ),
      Stats(
        8000,
        2.207e-7,
        1.932e-5,
        0.001728
      )
    )
}
