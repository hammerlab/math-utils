package quartic.complex

import org.hammerlab.math.polynomial.{ PolySolverTest, Stats, TestCase }
import spire.math.Complex
import spire.implicits._

class QuarticTest
  extends PolySolverTest[Double](4) {

  type D = Double

  val M = 6
  override val casePrintInterval = 1000

  def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d, e) = t.coeffs
    Quartic.doubleComplex[D].apply(a, b, c, d, e)
  }

  val sweep =
    results(
      absStats =
        Stats(
            n = 10920,
          max = 7.807e-8,
            μ = 4.457e-9,
            σ = 1.149e-8
        ),
      ratioStats =
        Stats(
            n = 10920,
          max = 7.097e-8,
            μ = 1.715e-9,
            σ = 5.299e-9
        ),
      numExpectedZeros = 1956
    )

  val random =
    results(
      absStats =
        Stats(
            n = 2700,
          max = 2.173e-4,
            μ = 2.998e-7,
            σ = 7.323e-6
        ),
      ratioStats =
        Stats(
            n = 2700,
          max = 0.002211,
            μ = 2.55e-6,
            σ = 7.366e-5
        )
    )
}
