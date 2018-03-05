package quartic.complex

import hammerlab.show._
import org.hammerlab.math.FromDouble
import org.hammerlab.math.polynomial.TestCase
import org.hammerlab.math.polynomial.test.PolySolverTest
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
   * The [[TestCase]]s with the worst numerical-imprecision artifacts tend to be polynomials of the form (x-r)³(x-r-ε);
   * the constant term of the depressed quartic is O(ε⁴), comes from subtracting terms that are O(r), and means that the
   * downstream precision is reduced to ¼ the number of digits that were maintained upstream.
   */
  ε = 1e-14
}
