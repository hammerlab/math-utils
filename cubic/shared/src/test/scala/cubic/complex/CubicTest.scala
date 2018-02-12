package cubic.complex

import org.hammerlab.math.polynomial.{ PolySolverTest, TestCase }
import spire.implicits._
import spire.math.Complex

abstract class CubicTest
  extends PolySolverTest[Double](3) {

  type D = Double

  val M = 6
  override val casePrintInterval = 500

  def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d) = t.coeffs
    Cubic.doubleComplex[D].apply(a, b, c, d)
  }
}
