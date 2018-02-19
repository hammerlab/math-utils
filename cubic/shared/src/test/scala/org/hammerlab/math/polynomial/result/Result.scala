package org.hammerlab.math.polynomial.result

import hammerlab.lines._
import hammerlab.show._
import org.hammerlab.math.polynomial.TestCase
import org.hammerlab.math.syntax.Doubleish._
import org.hammerlab.math.syntax.{ Doubleish, E }
import spire.algebra.{ Field, IsReal, NRoot, Trig }
import spire.math.{ Complex, max }

case class Result[D](tc: TestCase[D],
                     actual: Seq[Complex[D]],
                     maxAbsErr: Double,
                     maxErrRatio: Option[Double],
                     zeros: Zeros)

object Result {
  def apply[D: Field : Doubleish : IsReal : NRoot : Trig ](t: TestCase[D])(
      implicit
      ε: E,
      solve: TestCase[D] ⇒ Seq[Complex[D]]
  ): Result[D] = {
    val actual = solve(t)

    if (t.roots.size != actual.size)
      throw new IllegalArgumentException(
        s"Sizes differ: $actual vs ${t.roots}"
      )

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
      Zeros(
        numExpectedZeros,
        numActualZeros
      )
    )
  }

  import ResultGroup.showResultGroupOpt
  implicit def showResult[D: Show : Field : Ordering](implicit d: Show[Double], indent: Indent): ToLines[Result[D]] =
    ToLines {
      r ⇒ (r, None: Option[Int]).lines
    }
}
