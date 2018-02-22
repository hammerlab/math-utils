package cubic.complex

import hammerlab.scalajs._
import org.hammerlab.math.polynomial.result.Stats
import org.hammerlab.math.polynomial.roots.RootShapes
import org.hammerlab.math.polynomial.roots.dsl.IsRootShapes
import spire.implicits._

class DoubleTest
  extends CubicTest[Double] {
  override val casePrintInterval = 1000
  val M = 15
  val iterationsPerRootShape = 5000

  sweepTests(
    3 →
      Stats(
        93,
        0,
        0,
        0
      ),
    (2,1) →
      Stats(
        2790,
        4.89e-9,
        9.53e-9,
        7.3e-8
      ),
    (1,1,1) →
      Stats(
        13485,
        3.18e-16,
        9.37e-16,
        3.43e-14
      ),
    (1||1) →
      Stats(
        43245,
        3.14e-16,
        6.92e-16,
        3.82e-14
      )
  )

  val random =
    expected(
      Stats(
        20000,
        1.38e-6,
        2.78e-6,
        1.23e-5
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        20000,
        js{ 1.37e-6 }{ 1.38e-6 },
        js{ 2.77e-6 }{ 2.77e-6 },
        js{ 1.21e-5 }{ 1.15e-5 }
      )
    )
}
