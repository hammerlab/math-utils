package quartic.complex

import org.hammerlab.math.polynomial
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, PolySolverTest, Real, Root }
import org.hammerlab.math.syntax.{ Doubleish, E }
import org.hammerlab.math.syntax.FuzzyCmp.FuzzyCmpOps
import org.hammerlab.test.Cmp
import Doubleish.DoubleishOps
import spire.algebra.Ring
import spire.math.Complex
import spire.syntax.all._
import spire.implicits.DoubleAlgebra
import ImaginaryRootPair.ImaginaryRootPairOps

import Seq.fill

class QuarticTest
  extends PolySolverTest(4) {
  override type R[T] = Root[T]
  override type Real[T] = polynomial.Real[T]
  override def root[T](value: T, degree: Int): Seq[Real[T]] = fill(degree)(Real(value))
  override def M: Int = 6
  override def toComplex[T: Ring](r: Root[T]): Seq[Complex[T]] =
    r match {
      case Real(r) ⇒ Seq(Complex(r))
      case ImaginaryRootPair(a, b) ⇒ Seq(Complex(a, -b), Complex(a, b))
    }

  override val casePrintInterval: Int = 1000

  def expected[T: Doubleish](t: TestCase[T]): Seq[Complex[D]] = {
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
  }

  type Results = Seq[Complex[D]]
  case class ResultsCmp(msg: String, l: Results, r: Results) {
    override def toString: String =
      (
        msg ::
        l
          .zip(r)
          .map { case (l, r) ⇒ s"$l\t\t$r" }
          .toList
      )
      .mkString("\n\t")
  }

  import math.max
  implicit def complexCanEq(implicit ε: E): Cmp[Results] =
    Cmp[Results, ResultsCmp](
      (l, r) ⇒
        if (l.size != r.size)
          Some(
            ResultsCmp(
              s"Sizes don't match: $l vs $r",
              l,
              r
            )
          )
        else {
          val (aligned, (idx, maxErr, sum)) =
            r
              .permutations
              .map {
                r ⇒
                  r →
                    l
                      .zip(r)
                      .zipWithIndex
                      .map {
                        case ((l, r), idx) ⇒
                          (l - r).abs → idx
                      }
                      .foldLeft(
                        (
                           -1,  // index where maximum error was observed
                          0.0,  // maximum error observed
                          0.0   // sum of error across all elements
                        )
                      ) {
                        case (
                          (maxIdx, maxErr, sum),
                          (cur, idx)
                        ) ⇒
                          (
                            if (cur >= maxErr)
                              idx
                            else
                              maxIdx,
                            max(maxErr, cur),
                            sum + cur
                          )
                      }
              }
              .minBy(_._2._2)

          if (doubleCmp.eqv(maxErr, 0))
            None
          else {
            Some(
              ResultsCmp(
                s"Best alignment of complex sequences was still bad: err $maxErr at idx $idx, sum $sum, ε $ε",
                l,
                aligned
              )
            )
          }
        }
    )

  def check(t: TestCase[D])(implicit ε: E = ε): Unit =
    withClue(s"$t:\n") {
      val Seq(a, b, c, d, e) = t.coeffs
      val actual: Results = Quartic.doubleComplex(this.ε).apply(a, b, c, d, e)

      // Test that the solver returns the correct roots, given the coefficients
      ===(
        actual,
        expected(t)
      )
    }

  test("sweep") {
    rootSweep.foreach(check)
  }

  test("double and imag pair") {
    check(
      TestCase(
        Seq(Real(-6.0), Real(-6.0)),
        Seq(ImaginaryRootPair[D](-5, 2) → 1),
        -3.0
      )
    )
  }

  test("double imag pair") {
    check(
      TestCase(
        Nil,
        Seq(ImaginaryRootPair[D](-6, 1) → 2),
        -3.0
      )
    )
  }

  test("two imag pairs") {
    check(
      TestCase(
        Nil,
        Seq(
          ImaginaryRootPair[D](-6, 1) → 1,
          ImaginaryRootPair[D](-5, 1) → 1
        ),
        -3.0
      )
    )

    check(
      TestCase(
        Nil,
        Seq(
          ImaginaryRootPair[D](-6, 1) → 1,
          ImaginaryRootPair[D](-5, 2) → 1
        ),
        -3.0
      )
    )
  }

  test("imag quad") {
    check(
      TestCase(
        Seq(Real(-6.0), Real(-6.0)),
        Seq(ImaginaryRootPair[D](-6, 1) → 1),
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
    )(
      1e-4
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
  }
}
