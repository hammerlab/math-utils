package cubic.complex

import hammerlab.scalajs._
import org.hammerlab.math.polynomial.result.Stats
import spire.implicits._

class DoubleTest
  extends CubicTest[Double] {
  override val casePrintInterval = 10000

  val M = 15
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
        3.14e-16 js 2.51e-16,
        6.92e-16,
        3.82e-14
      )
  )

  val iterationsPerRootShape = 5000
  gaussians(
    3 →
      Stats(
        15000,
        4.37e-6,
        2.93e-6,
        1.11e-5
      ),
    (2,1) →
      Stats(
        15000,
        9.05e-9 js 8.95e-9,
        2.17e-8 js 1.92e-8,
        9.09e-7 js 4.72e-7
      ),
    (1,1,1) →
      Stats(
        15000,
        1.67e-15,
        2.57e-14,
        1.38e-12
      ),
    (1||1) →
      Stats(
        15000,
        1.36e-15 js 1.87e-15,
        2.61e-14 js 7.21e-14,
        1.72e-12 js 6.07e-12
      )
  )

  logNormals(
    3 →
      Stats(
        15000,
        4.41e-6 js 4.36e-6,
        2.94e-6 js 2.92e-6,
        1.15e-5 js 1.17e-5
      ),
    (2,1) →
      Stats(
        15000,
        8.76e-9 js 8.86e-9,
        1.98e-8 js 1.92e-8,
        7.23e-7 js 4.99e-7
      ),
    (1,1,1) →
      Stats(
        15000,
        1.45e-15 js 2.19e-15,
        2.27e-14 js 5.87e-14,
        1.44e-12 js 4.87e-12
      ),
    (1||1) →
      Stats(
        15000,
        4.65e-16 js 4.14e-16,
        1.36e-15 js 1.63e-15,
        5.94e-14 js 1.01e-13
      )
  )
}
