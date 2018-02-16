package org.hammerlab.math.polynomial

import java.math.MathContext

import hammerlab.show._
import org.hammerlab.math.syntax.{ Doubleish, E }
import Doubleish._
import spire.algebra.{ Field, IsReal, NRoot, Order, Trig }
import spire.math.Complex
import spire.math.{ log, max, min }
import spire.implicits._

sealed trait Ratio
object Ratio {
  def apply[D: Doubleish : Field : Trig : Order : IsReal : NRoot](l: Complex[D], r: Complex[D]): Ratio = {
    (l.abs, r.abs, (l - r).abs) match {
      case (0, 0, _) ⇒ Log(0)
      case (0, _, _) ⇒ LeftZero
      case (_, 0, _) ⇒ RightZero
      case (l, r, d) ⇒
        Log(
          max(
            log(l + d) - log(l),
            log(r + d) - log(r)
          )
          .toDouble
        )
    }
  }

  implicit val ord: Ordering[Ratio] =
    new Ordering[Ratio] {
      override def compare(x: Ratio, y: Ratio): Int =
        (x, y) match {
          case (            LeftZero,            RightZero) ⇒  1
          case (           RightZero,             LeftZero) ⇒ -1
          case (LeftZero | RightZero,               Log(_)) ⇒  1
          case (              Log(_), LeftZero | RightZero) ⇒ -1
          case (              Log(l),               Log(r)) ⇒  Ordering[Double].compare(l, r)
          case _ ⇒ 0
        }
    }
}
case object       LeftZero extends Ratio
case object      RightZero extends Ratio
case  class Log(v: Double) extends Ratio

case class Result[D](tc: TestCase[D],
                     actual: Seq[Complex[D]],
                     maxAbsErr: Double,
                     maxErrRatio: Option[Double],
                     numExpectedZeros: Int,
                     numActualZeros: Int)

object Result {
  def apply[D: Field : Doubleish : IsReal : NRoot : Trig ](t: TestCase[D])(
      implicit
      ε: E,
      solve: TestCase[D] ⇒ Seq[Complex[D]]
  ): Result[D] = {
    val actual = solve(t)

//    println(s"actual: $actual ${actual.map(_.asInstanceOf[Complex[BigDecimal]].real.mc)}")

    if (t.roots.size != actual.size)
      throw new IllegalArgumentException(
        s"Sizes differ: $actual vs ${t.roots}"
      )

    def mc(d: D): MathContext = d.asInstanceOf[BigDecimal].mc
//    def mc(d: Complex[D]): (MathContext, MathContext) = {
//      val Complex(r, i) = d.asInstanceOf[Complex[BigDecimal]]
//      (r.mc, i.mc)
//    }

    /**
     * Find the bijection between expected and actual roots that minimizes the maximum difference between them (as
     * measured by the [[spire.math.Complex.abs abs]] of their ([[Complex]]) difference).
     *
     * Roots are considered to be unordered on the input and output ends, and simple ordering heuristics (e.g.
     * separating real vs imaginary roots) tend to fail due to numerical imprecision (bona-fide real roots can be
     * returned as having small imaginary components due to floating-point error, with some roots-configurations
     * especially exacerbating this).
     */
    val (aligned, (maxAbsErr, maxErrRatio, numExpectedZeros, numActualZeros)) =
      actual
        .permutations
        .map {
          r ⇒
//            println(s"perm: $r")
            r →
              t
                .roots
                .zip(r)
                .map {
                  case (l, r) ⇒
                    (
                      (l - r).abs,  // absolute error
                      Ratio(l, r)   // error ratio
                    )
                }
                .foldLeft(
                  (
                    0.0,                   // maximum absolute error
                    None: Option[Double],  // maximum log(error ratio); considered to be "not present" if e.g. all mappings are asymmetric zeros
                    0,                     // number of {expected-zero, actual non-zero}s
                    0                      // number of {expected-nonzero, actual zero}s
                  )
                ) {
                  case (
                    (maxAbsErr, maxRatio, numExpectedZeros, numActualZeros),
                    (abs, ratio)
                  ) ⇒
                    val absErr =
                      max(
                        maxAbsErr,
                        abs.toDouble
                      )

                    ratio match {
                      case LeftZero ⇒
                        (absErr, maxRatio, numExpectedZeros + 1, numActualZeros)
                      case RightZero ⇒
                        (absErr, maxRatio, numExpectedZeros, numActualZeros + 1)
                      case Log(r) ⇒
                        (absErr, maxRatio.map(max(_, r)).orElse(Some(r)), numExpectedZeros, numActualZeros)
                    }
                }
        }
        .minBy(_._2)

    Result(
      t,
      aligned,
      maxAbsErr,
      maxErrRatio,
      numExpectedZeros,
      numActualZeros
    )
  }

  implicit def showComplex[D: Show : Field : Ordering](implicit d: Show[Double]): Show[Complex[D]] =
    Show {
      case Complex(r, i) ⇒
        if (i == 0)
          show"$r"
        else if (Ordering[D].lt(i, Field[D].zero))
          show"$r${i}i"
        else
          show"$r+${i}i"
    }

  implicit def showResult[D: Show : Field : Ordering](implicit d: Show[Double]): Show[(Result[D], Int)] =
    Show {
      case (Result(tc, actual, maxAbsErr, maxErrRatio, numExpectedZeros, numActualZeros), num) ⇒

        val parenthetical =
          if (numExpectedZeros > 0 || numActualZeros > 0)
            show"$numExpectedZeros,$numActualZeros mismatched zeros; $num copies"
          else
            show"$num copies"

        (
          show"max err: $maxAbsErr abs, ${maxErrRatio.fold("NaN")(_.show)} ratio, scale ${tc.scale} ($parenthetical)" ::
          tc
            .roots
            .zip(actual)
            .map { case (e, a) ⇒ show"$e\t\t$a" }
            .toList
        )
        .mkString("\n\t")
    }
}

