package quartic.complex

import hammerlab.math.FromDouble
import hammerlab.indent.implicits.spaces4
import hammerlab.lines._
import hammerlab.show._
import org.hammerlab.math.polynomial.result.Result
import org.hammerlab.math.polynomial.test.PolySolverTest
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, Real, TestCase }
import org.hammerlab.math.syntax.Doubleish
import quartic.complex.Quartic.HasQuartic
import spire.algebra.{ Field, IsReal, NRoot, Signed, Trig }
import spire.implicits._
import spire.math.Complex

abstract class QuarticTest[D: HasQuartic : Ordering : FromDouble : Field : IsReal : NRoot : Signed : Trig : Doubleish]
  extends PolySolverTest[D](4) {
  implicit def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d, e) = t.coeffs
    implicitly[HasQuartic[D]].apply.apply(a, b, c, d, e)
  }
}

abstract class BigDecimalTest
  extends QuarticTest[BigDecimal] {

  /**
   * This controls the sensitivity of marking depressed-quartic coefficients as "zero"
   *
   * The [[TestCase]]s with the worst numerical-imprecision artifacts tend to be polynomials of the form (x-r)(x-r-ε);
   * the constant term of the depressed quartic is O(ε⁴), and comes from subtracting terms that are O(r).
   */
  ε = 1e-14
}
