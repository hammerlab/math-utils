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
        3.1e-16,
        6.9e-16,
        3.8e-14
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
        9.1e-9,
        2.2e-8,
        9.1e-7
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
        1.4e-15,
        2.6e-14,
        1.7e-12
      )
  )

  logNormals(
    3 →
      Stats(
        15000,
        4.41e-6 js_? 4.36e-6,
        2.94e-6 js_? 2.92e-6,
        1.15e-5 js_? 1.17e-5
      ),
    (2,1) →
      Stats(
        15000,
        8.8e-9,
        2e-8,
        7.2e-7
      ),
    (1,1,1) →
      Stats(
        15000,
        1.5e-15 js_? 1.6e-15,
        2.3e-14 js_? 2.5e-14,
        1.4e-12
      ),
    (1||1) →
      Stats(
        15000,
        4.7e-16 js_? 4.8e-16,
        1.4e-15 js_? 1.8e-15,
        5.9e-14 js_? 1e-13
      )
  )
}
