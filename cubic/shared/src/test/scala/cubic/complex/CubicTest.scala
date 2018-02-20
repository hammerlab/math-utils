package cubic.complex

import hammerlab.math.FromDouble
import org.hammerlab.math.polynomial.{ PolySolverTest, TestCase }
import org.hammerlab.math.syntax.Doubleish
import spire.algebra.{ Field, IsReal, NRoot, Signed, Trig }
import spire.implicits._
import spire.math.Complex

abstract class CubicTest[D : Ordering : FromDouble : Field : IsReal : NRoot : Signed : Trig : Doubleish](implicit cubic: Cubic.FromEpsilon[D])
  extends PolySolverTest[D](3) {
  implicit def solve(t: TestCase[D]): Seq[Complex[D]] = {
    val Seq(a, b, c, d) = t.coeffs
    cubic.apply.apply(a, b, c, d)
  }
}

abstract class BigDecimalTest
  extends CubicTest[BigDecimal]
