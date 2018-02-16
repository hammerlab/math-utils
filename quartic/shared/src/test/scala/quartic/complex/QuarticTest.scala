package quartic.complex

import org.hammerlab.math.polynomial.{ PolySolverTest, TestCase }
import spire.implicits._
import spire.math.Complex

abstract class QuarticTest
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
}

abstract class BigDecimalTest
  extends PolySolverTest[BigDecimal](4) {

  /**
   * This controls the sensitivity of marking depressed-quartic coefficients as "zero"
   *
   * The [[TestCase]]s with the worst numerical-imprecision artifacts tend to be polynomials of the form (x-r)(x-r-ε);
   * the constant term of the depressed quartic is O(ε⁴), and comes from subtracting terms that are O(r).
   */
  ε = 1e-8

  val M = 6
  override val casePrintInterval = 1

  implicit def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d, e) = t.coeffs
    Quartic.doubleComplex[D].apply(a, b, c, d, e)
  }

  type D = BigDecimal
}
