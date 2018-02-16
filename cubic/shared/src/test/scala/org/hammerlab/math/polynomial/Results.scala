package org.hammerlab.math.polynomial

import cats.Show
import cats.Show.show
import cats.implicits.catsStdShowForInt
import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.math.syntax.{ Doubleish, E }
import spire.algebra.{ Field, IsReal, NRoot, Trig }
import spire.math.Complex
import spire.math.sqrt

import math.max

case class Results[D](results: Seq[(Result[D], Int)],
                      absStats: Stats,
                      ratioStats: Stats,
                      numExpectedZeros: Int,
                      numActualZeros: Int)

case class Stats(n: Int, μ: Double, σ: Double, max: Double)

object Stats {
  def apply(elems: Iterable[Double]): Stats = {
    val (n, sum, sqs, mx) =
      elems
        .foldLeft(
          (
            0,    // num elements
            0.0,  // sum
            0.0,  // sum of squares
            0.0   // maximum
          )
        ) {
        case ((n, sum, sqs, mx), cur) ⇒
          (
            n + 1,
            sum + cur,
            sqs + cur*cur,
            max(mx, cur)
          )
      }

    val μ = sum / n
    val σ = sqrt(sqs/n - μ*μ)
    Stats(n, μ, σ, mx)
  }

  implicit def showNormal(implicit s: Show[Double]): Show[Stats] =
    show {
      case Stats(n, μ, σ, max) ⇒
        show"n $n μ $μ σ $σ max $max"
    }
}

object Results {
  def apply[D: Field : Doubleish : IsReal : NRoot : Trig](cases: Iterator[TestCase[D]])(
      implicit
      ε: E,
      solve: TestCase[D] ⇒ Seq[Complex[D]]
  ): Results[D] = {
    val results =
      cases
        .map(Result(_))
        .toVector
        .sortBy(r ⇒ r.maxErrRatio → r.maxAbsErr)
        .reverse

    val grouped =
      results
        .runLengthPartial {
          case (cur, next)
            if ((cur.maxErrRatio, cur.maxAbsErr) == (next.maxErrRatio, next.maxAbsErr)) ⇒
            cur
        }
        .toVector

    Results(
      grouped,
      Stats(results.    map(_.maxAbsErr)),
      Stats(results.flatMap(_.maxErrRatio)),
      results.map(_.numExpectedZeros).sum,
      results.map(_.numActualZeros).sum
    )
  }

  implicit def showResults[D: Show : Field : Ordering](implicit s: Show[Double]): Show[Results[D]] =
    show {
      case Results(
        results,
        absStats,
        ratioStats,
        numExpectedZeros,
        numActualZeros
      ) ⇒
        val mismatchedZeros =
          if (numExpectedZeros > 0 || numActualZeros > 0)
            show" ($numExpectedZeros,$numActualZeros mismatched zeros)"
          else
            ""

        import cats.implicits.catsStdShowForString
        (
          show"abs: $absStats ratios: $ratioStats$mismatchedZeros" ::
          results.take(3).map(_.show).toList
        )
        .mkString("\n", "\n\t", "\n")
    }
}

