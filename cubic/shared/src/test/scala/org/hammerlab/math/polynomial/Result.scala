package org.hammerlab.math.polynomial

import cats.Show
import cats.Show.show
import cats.implicits.catsStdShowForInt
import cats.syntax.show._
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.syntax.{ Doubleish, E }
import Doubleish._
import spire.algebra.{ Field, IsReal, NRoot }
import spire.math.Complex
import spire.implicits._
import spire.math.max

case class Result[D](tc: TestCase[D],
                     actual: Seq[Complex[D]],
                     maxErr: Double)

object Result {
  def apply[D: Field : Doubleish : IsReal : NRoot](t: TestCase[D])(
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
    val (aligned, maxErr) =
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
                    (l - r).abs
                }
                .foldLeft(
                  0.0  // maximum error observed
                ) {
                  case (
                    maxErr,
                    cur
                  ) ⇒
                    max(
                      maxErr,
                      cur.toDouble
                    )
                }
        }
        .minBy(_._2)

    Result(
      t,
      aligned,
      maxErr
    )
  }

  import SigFigs.showSigFigs
  implicit def showComplex[D: Show : Field : Ordering](implicit sf: SigFigs): Show[Complex[D]] =
    show {
      case Complex(r, i) ⇒
        if (i == 0)
          show"$r"
        else if (Ordering[D].lt(i, Field[D].zero))
          show"$r${i}i"
        else
          show"$r+${i}i"
    }

  implicit def showResult[D: Show : Field : Ordering](implicit sf: SigFigs): Show[(Result[D], Int)] =
    show {
      case (Result(tc, actual, maxErr), num) ⇒
        (
          show"max err: $maxErr ($num copies)" ::
          tc
            .roots
            .zip(actual)
            .map { case (e, a) ⇒ show"$e\t\t$a" }
            .toList
        )
        .mkString("\n\t")
    }
}

