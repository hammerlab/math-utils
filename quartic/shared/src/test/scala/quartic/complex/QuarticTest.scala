package quartic.complex

import hammerlab.show._
import org.hammerlab.math.polynomial.result.Result
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, PolySolverTest, Real, TestCase }
import spire.implicits._
import spire.math.Complex

abstract class QuarticTest
  extends PolySolverTest[Double](4) {

  test("bad") {
    val result: Result[D] =
      Result(
        TestCase[D](
          Seq(Real[D](3) → 2),
          Seq(ImaginaryRootPair[D](-3, 1) → 1),
          1
        )
      )

    import Result.showResult

    println((result → 1).show)
  }

  /**
   * This controls the sensitivity of marking depressed-quartic coefficients as "zero"
   *
   * The [[TestCase]]s with the worst numerical-imprecision artifacts tend to be polynomials of the form (x-r)(x-r-ε);
   * the constant term of the depressed quartic is O(ε⁴), and comes from subtracting terms that are O(r).
   */
  ε = 1e-8

  val M = 3
  override val casePrintInterval = 100

  implicit def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d, e) = t.coeffs
    Quartic.doubleComplex[D].apply(a, b, c, d, e)
  }
}

abstract class BigDecimalTest
  extends PolySolverTest[BigDecimal](4) {

  /**
   * This controls the sensitivity of marking depressed-quartic coefficients as "zero"
   *
   * The [[TestCase]]s with the worst numerical-imprecision artifacts tend to be polynomials of the form (x-r)(x-r-ε);
   * the constant term of the depressed quartic is O(ε⁴), and comes from subtracting terms that are O(r).
   */
  ε = 1e-14

  val M = 3
  override val casePrintInterval = 1

  implicit def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d, e) = t.coeffs
    Quartic.doubleComplex[D].apply(a, b, c, d, e)
  }

  test("bad") {
    val result: Result[D] =
      Result(
        TestCase[D](
          Seq(Real[D](3) → 2),
          Seq(ImaginaryRootPair[D](-3, 1) → 1),
          1
        )
      )

    import Result.showResult

    println((result → 1).show)
  }
}
