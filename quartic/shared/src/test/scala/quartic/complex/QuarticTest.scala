package quartic.complex

import org.hammerlab.math.polynomial
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, PolySolverTest, Real, Result }
import org.hammerlab.math.syntax.Doubleish
import Doubleish.DoubleishOps
import org.hammerlab.math.syntax.FuzzyCmp.FuzzyCmpOps
import org.hammerlab.test.CanEq
import spire.algebra.Ring
import spire.math.Complex
import spire.syntax.all._

import Seq.fill

class QuarticTest
  extends PolySolverTest(4) {
  override type R[T] = Result[T]
  override type Real[T] = polynomial.Real[T]
  override def root[T](value: T, degree: Int): Seq[Real[T]] = fill(degree)(Real(value))
  override def M: Int = 6
  override def toComplex[T: Ring](r: Result[T]): Seq[Complex[T]] =
    r match {
      case Real(r) ⇒ Seq(Complex(r))
      case ImaginaryRootPair(a, b) ⇒ Seq(Complex(a, -b), Complex(a, b))
    }

  ε = 1e-6

  override val casePrintInterval: Int = 1

  import ImaginaryRootPair.ImaginaryRootPairOps

  def expected[T: Doubleish](t: TestCase[T]): Seq[R[Dbl]] =
    t
      .reals
      .map {
        case Real(r) ⇒ Real(r.toDouble)
      } ++
    t
      .imags
      .flatMap {
        case (i @ ImaginaryRootPair(a, b), d) ⇒
          fill(d)(ImaginaryRootPair(a.toDouble, b.toDouble))
      }

  def check(t: TestCase[Dbl]): Unit =
    withClue(s"$t:\n") {
      val Seq(a, b, c, d, e) = t.coeffs
      val actual: Seq[R[Dbl]] = Quartic.doubleResult.apply(a, b, c, d, e)

      // Test that the solver returns the correct roots, given the coefficients
      ===(
        actual,
        expected(t)
      )
    }

  test("sweep") {
    rootSweep.foreach(check)
  }

  import spire.implicits.DoubleAlgebra

  test("double and imag pair") {
    check(
      TestCase(
        Seq(Real(-6.0), Real(-6.0)),
        Seq(ImaginaryRootPair[Dbl](-5, 2) → 1),
        -3.0
      )
    )
  }

  test("double imag pair") {
    check(
      TestCase(
        Nil,
        Seq(ImaginaryRootPair[Dbl](-6, 1) → 2),
        -3.0
      )
    )
  }

  test("two imag pairs") {
    check(
      TestCase(
        Nil,
        Seq(
          ImaginaryRootPair[Dbl](-6, 1) → 1,
          ImaginaryRootPair[Dbl](-5, 1) → 1
        ),
        -3.0
      )
    )

    check(
      TestCase(
        Nil,
        Seq(
          ImaginaryRootPair[Dbl](-6, 1) → 1,
          ImaginaryRootPair[Dbl](-5, 2) → 1
        ),
        -3.0
      )
    )
  }

  test("imag quad") {
    check(
      TestCase(
        Seq(Real(-6.0), Real(-6.0)),
        Seq(ImaginaryRootPair[Dbl](-6, 1) → 1),
        -3.0
      )
    )
  }

  test("hard triple") {
    check(
      TestCase(
        Seq(Real(0.7056936492337609), Real(0.711645855752505), Real(0.711645855752505), Real(0.711645855752505)),
        Nil,
        -0.8856759931799498
      )
    )
  }

  test("random roots") {
    import Real.doubleish
    randomCases(doubleish).foreach(check)
/*
    for {
      t @ TestCase(_, _, _, Seq(a, b, c, d, e)) <- randomCases(doubleish)
    } withClue(s"$t:\n") {
      ===(
        Quartic.doubleResult.apply(a, b, c, d, e),
        expected(t)
      )
    }
*/
  }
}
