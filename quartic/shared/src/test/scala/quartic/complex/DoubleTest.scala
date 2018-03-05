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

  val M = 7

  sweepTests(
    (4)        → Stats(    60,       0,       0,       0 ),
    (3,1)      → Stats(   840,       0,       0,       0 ),
    (2,2)      → Stats(   420,       0,       0,       0 ),
    (2,1,1)    → Stats(  5460, 1.4e-09, 4.1e-09, 4.1e-08 ),
    (1,1,1,1)  → Stats(  5460, 1.2e-15, 3.4e-15, 4.1e-14 ),
    (2||1)     → Stats(  6300, 1.6e-09, 3.7e-09, 5.6e-08 ),
    ((1,1)||1) → Stats( 44100, 2.1e-16, 3.6e-16, 1.3e-14 ),
    ||(2)      → Stats(   420,       0,       0,       0 ),
    ||(1,1)    → Stats( 21840, 3.9e-16, 8.8e-16, 8.2e-15 )
  )

  val iterationsPerRootShape: Int = 2000
  gaussians(
    (4)        → Stats( 8000, 5.2e-16, 1.2e-15, 2.0e-14 ),
    (3,1)      → Stats( 8000, 4.7e-06, 5.5e-06, 5.0e-05 ),
    (2,2)      → Stats( 8000, 7.5e-08, 1.1e-06, 4.8e-05 ),
    (2,1,1)    → Stats( 8000, 1.5e-08, 8.1e-08, 3.8e-06 ),
    (1,1,1,1)  → Stats( 8000, 7.1e-13, 1.8e-11, 7.9e-10 ),
    (2||1)     → Stats( 8000, 1.1e-08, 1.1e-07, 6.7e-06 ),
    ((1,1)||1) → Stats( 8000, 2.3e-15, 2.8e-14, 1.1e-12 ),
    ||(2)      → Stats( 8000, 6.2e-08, 5.4e-07, 2.2e-05 ),
    ||(1,1)    → Stats( 8000, 1.3e-13, 3.3e-12, 1.3e-10 )
  )

  logNormals(
    (4) →
      Stats(
        8000,
        1.3e-14 js 1.2e-14,
        1.5e-13 js 1.5e-13,
        5.5e-12 js 5.3e-12
      ),
    (3,1) →
      Stats(
        8000,
        4.3e-6,
        5.1e-6 js 5.2e-6,
        5.4e-5 js 6.5e-5
      ),
    (2,2) →
      Stats(
        8000,
        3.4e-8,
        1.3e-7,
        2.2e-6
      ),
    (2,1,1) →
      Stats(
        8000,
        1.2e-8 js 1.1e-8,
        8.2e-8 js 4.1e-8,
        4.4e-6 js 1.7e-6
      ),
    (1,1,1,1) →
      Stats(
        8000,
        3.9e-13,
        9.3e-12,
        4.7e-10
      ),
    (2||1) →
      Stats(
        8000,
        7.8e-9,
        2.1e-8,
        5.9e-7
      ),
    ((1,1)||1) →
      Stats(
        8000,
        1.9e-15,
        1.9e-14,
        6e-13
      ),
    ||(2) →
      Stats(
        8000,
        3e-8,
        8.1e-8 js 8.6e-8,
        1.8e-6
      ),
    ||(1,1) →
      Stats(
        8000,
        8.4e-14,
        2.3e-12,
        1e-10
      )
  )
}
