package cubic.complex

import cats.implicits.catsStdShowForInt
import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.math.format.SigFigs.showSigFigs
import org.hammerlab.math.polynomial.{ PolySolverTest, Results, TestCase }
import org.hammerlab.math.syntax.E
import spire.math.Complex

abstract class CubicTest
  extends PolySolverTest(3) {

  val M: Int = 6
  override val casePrintInterval: Int = 500

  implicit def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d) = t.coeffs
    Cubic.doubleComplex.apply(a, b, c, d)
  }

  def check(cases: Iterator[TestCase[D]],
                n: Int,
              max: D,
                μ: D,
                σ: D
           ): Unit = {
    val results = Results(cases)
    println(results.show)

    ===(results.  n, n)

    {
      implicit val ε: E = 1e-3
      ===(results.max, max)
      ===(results.  μ,   μ)
      ===(results.  σ,   σ)
    }
  }
}
