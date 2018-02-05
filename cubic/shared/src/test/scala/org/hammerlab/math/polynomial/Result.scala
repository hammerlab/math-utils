package org.hammerlab.math.polynomial

import cats.Show
import cats.Show.show
import cats.implicits.catsStdShowForInt
import cats.syntax.show._
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.syntax.E
import spire.math.Complex
import spire.implicits._

import scala.math.max

case class Result[D](tc: TestCase[D],
                     actual: Seq[Complex[D]],
                     maxErr: D)

object Result {
  type D = Double
  def apply(t: TestCase[D])(
      implicit
      ε: E,
      solve: TestCase[D] ⇒ Seq[Complex[D]]
  ): Result[D] = {
    val actual = solve(t)

    if (t.roots.size != actual.size)
      throw new IllegalArgumentException(
        s"Sizes differ: $actual vs ${t.roots}"
      )

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
                    max(maxErr, cur)
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
  implicit def showComplex(implicit sf: SigFigs): Show[Complex[D]] =
    show {
      case Complex(r, i) ⇒
        if (i == 0)
          show"$r"
        else if (i < 0)
          show"$r${i}i"
        else
          show"$r+${i}i"
    }

  implicit def showResult(implicit sf: SigFigs): Show[(Result[D], Int)] =
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

