package quartic.complex

import org.hammerlab.math.polynomial
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, PolySolverTest, Real, Result }
import org.hammerlab.math.syntax.{ Doubleish, Tolerance }
import cats.Eq
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

/*
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
*/

  import Doubleish.DoubleishOps
  def expected[T: Doubleish](t: TestCase[T]): Seq[Complex[Dbl]] = {
    import spire.implicits.DoubleAlgebra
    (
      t
        .reals
        .map {
          case Real(r) ⇒
            Complex(r.toDouble)
        } ++
      t
        .imags
        .flatMap {
          case (i @ ImaginaryRootPair(a, b), d) ⇒
            fill(d)(ImaginaryRootPair(a.toDouble, b.toDouble))
        }
        .flatMap(_.complex)
    )
    //.sortBy(_.abs)
  }

/*
  implicit def complexCanEq(implicit ε: Tolerance): CanEq[Complex[Dbl], Complex[Dbl]] =
    new CanEq[Complex[Dbl], Complex[Dbl]] {
      override def eqv(x: Complex[Dbl], y: Complex[Dbl]): Boolean = {
        //x.asPolarTuple
        //        new FuzzyCmpOps(x.abs).===(y.abs) &&
        val req = new FuzzyCmpOps(x.real).===(y.real)
        val ieq = new FuzzyCmpOps(x.imag).===(y.imag)
        println(s"cmp: $x $y, $req $ieq")
        req && ieq
      }
    }
*/

//  implicit val complexOrd: Ordering[Complex[Dbl]] =
//    new Ordering[Complex[Dbl]] {
//      override def compare(x: Complex[Dbl], y: Complex[Dbl]): Int = ???
//    }

  import spire.implicits.DoubleAlgebra

  import math.max
  implicit def complexCanEq(implicit ε: Tolerance): CanEq[Seq[Complex[Dbl]], Seq[Complex[Dbl]]] =
    new CanEq[Seq[Complex[Dbl]], Seq[Complex[Dbl]]] {
      override type Error = (String, Seq[Complex[Dbl]], Seq[Complex[Dbl]])
      override def eqv(t: Seq[Complex[Dbl]], u: Seq[Complex[Dbl]]): Option[Error] =
        if (t.size != u.size)
          Some((s"Sizes don't match: $t vs $u", t, u))
        else {
          val (r, (idx, maxErr, sum)) =
            u
              .permutations
              .map {
                r ⇒
                  r →
                    t
                      .zip(r)
                      .zipWithIndex
                      .map {
                        case ((l, r), idx) ⇒ (l - r).abs → idx
                      }
                      .foldLeft((-1, 0.0, 0.0)) {
                        case ((prevIdx, maxErr, sum), (cur, idx)) ⇒
                          (
                            if (cur >= M)
                              idx
                            else
                              prevIdx,
                            max(maxErr, cur),
                            sum + cur
                          )
                      }
              }
              .minBy(_._2._2)

          if (doubleCanEq.eqv(maxErr, 0).isEmpty)
            None
          else {
            Some(
              (
                s"Best alignment of complex sequences was still bad: err $maxErr at idx $idx, sum $sum, ε $ε",
                t,
                r
              )
            )
          }
          //if (new FuzzyCmpOps(maxErr).=== 0)
        }
    }

  def check(t: TestCase[Dbl])(implicit ε: Tolerance = ε): Unit =
    withClue(s"$t:\n") {
      val Seq(a, b, c, d, e) = t.coeffs
      //val actual: Seq[R[Dbl]] = Quartic.doubleResult.apply(a, b, c, d, e)
      val actual: Seq[Complex[Dbl]] = {
        import spire.implicits.DoubleAlgebra
        Quartic.doubleComplex(this.ε).apply(a, b, c, d, e)//.sortBy(_.abs)
      }

      println(s"caneq: ${implicitly[CanEq[Complex[Dbl], Complex[Dbl]]]}")

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

  test("simple hard triple") {
    check(
      TestCase(
        Seq(Real(1.0), Real(1.0), Real(1.0), Real(1.01)),
        Nil,
        1.0
      )
    )(
      1e-4
    )
  }

  test("random roots") {
    import Real.doubleish
    randomCases(doubleish).foreach(check(_)(1e-3))
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
