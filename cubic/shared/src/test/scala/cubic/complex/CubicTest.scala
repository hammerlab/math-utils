package cubic.complex

import org.hammerlab.math.polynomial
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, PolySolverTest, Real, Result }
import org.hammerlab.math.syntax.Doubleish, Doubleish.DoubleishOps
import spire.algebra.Ring
import spire.math.Complex
import spire.syntax.all._
import Real.doubleish

import scala.Seq.fill

class CubicTest
  extends PolySolverTest(3) {

  ε = 1e-6

//  override val casePrintInterval: Int = 1

  def expected[T: Doubleish](t: TestCase[T]): Seq[R[Dbl]] =
    t.reals
      .map {
        case Real(r) ⇒ Real(r.toDouble)
      } ++
    t.imags
      .map {
        case (ImaginaryRootPair(a, b), _) ⇒
          ImaginaryRootPair[Dbl](a.toDouble, b.toDouble)
      }

  test("sweep") {
    for {
      t @ TestCase(_, _, _, Seq(a, b, c, d)) ← rootSweep
    } withClue(s"$t:\n") {
      val actual: Seq[R[Dbl]] = Cubic.doubleResult.apply(a, b, c, d)

      // Test that the solver returns the correct roots, given the coefficients
      ===(
        actual,
        expected(t)
      )
    }
  }

  test("random roots") {
    for {
      t @ TestCase(_, _, _, Seq(a, b, c, d)) <- randomCases(doubleish(Doubleish.id))
    } withClue(s"$t:\n") {
      ===(
        Cubic.doubleResult.apply(a, b, c, d),
        expected(t)
      )
    }
  }


  override type Real[T] = polynomial.Real[T]

  override def root[T](value: T, degree: Int): Seq[Real[T]] = fill(degree)(Real(value))

  override def toComplex[T: Ring](r: Result[T]): Seq[Complex[T]] =
    r match {
      case Real(r) ⇒ Seq(Complex(r))
      case ImaginaryRootPair(a, b) ⇒ Seq(Complex(a, -b), Complex(a, b))
    }

  type R[T] = Result[T]

  val M: Int = 6
}
