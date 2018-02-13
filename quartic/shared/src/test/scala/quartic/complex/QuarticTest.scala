package quartic.complex

import org.hammerlab.math.polynomial.{ PolySolverTest, Stats, TestCase }
import spire.implicits._
import spire.math.Complex

class QuarticTest
  extends PolySolverTest[Double](4) {

  /**
   * This controls the sensitivity of marking depressed-quartic coefficients as "zero"
   *
   * The [[TestCase]]s with the worst numerical-imprecision artifacts tend to be polynomials of the form (x-r)(x-r-ε);
   * the constant term of the depressed quartic is O(ε⁴), and comes from subtracting terms that are O(r).
   */
  ε = 1e-8

  val M = 6
  override val casePrintInterval = 1000

  implicit def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d, e) = t.coeffs
    Quartic.doubleComplex[D].apply(a, b, c, d, e)
  }

  type D = Double

  val sweep =
    results(
      absStats =
        Stats(
            n = 10920,
            μ = 4.457e-9,
            σ = 1.149e-8,
          max = 7.807e-8
        ),
      ratioStats =
        Stats(
            n = 10920,
            μ = 1.715e-9,
            σ = 5.299e-9,
          max = 7.097e-8
        ),
      numExpectedZeros = 1956
    )

  val random =
    results(
      absStats =
        Stats(
            n = 2700,
            μ = 8.062e-8,
            σ = 1.382e-6,
          max = 3.899e-5
        ),
      ratioStats =
        Stats(
            n = 2700,
            μ = 1.129e-7,
            σ = 1.695e-6,
          max = 4.688e-5
        )
    )

  val logNormalRandom =
    results(
      absStats =
        Stats(
            n = 2700,
            μ = 4.005e-8,
            σ = 2.919e-7,
          max = 7.549e-6
        ),
      ratioStats =
        Stats(
            n = 2700,
            μ = 2.331e-8,
            σ = 4.625e-8,
          max = 6.75e-7
        )
    )
}
