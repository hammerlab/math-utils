package quartic.complex

import org.hammerlab.math.polynomial.{ PolySolverTest, TestCase }
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

  sigfigs = 5

  test("sweep") {
    check(
      rootSweep,
        n = 10920,
      max = 7.807e-8,
        μ = 4.457e-9,
        σ = 1.149e-8
    )
  }


  test("random roots") {
    check(
      randomCases,
        n = 2700,
      max = 2.173e-4,
        μ = 3.103e-7,
        σ = 7.373e-6
    )
  }
}
