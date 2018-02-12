package org.hammerlab.math.polynomial

import cats.Show
import cats.Show.show
import cats.implicits.catsStdShowForInt
import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.format.SigFigs.showSigFigs
import org.hammerlab.math.syntax.{ Doubleish, E }
import spire.algebra.{ Field, IsReal, NRoot }
import spire.math.Complex
import spire.implicits._
import spire.math.sqrt

case class Results[D](results: Seq[(Result[D], Int)],
                      n: Int,
                      μ: Double,
                      σ: Double) {
  def max = results.head._1.maxErr
}

object Results {
  def apply[D: Field : Doubleish : IsReal : NRoot](cases: Iterator[TestCase[D]])(
      implicit
      ε: E,
      solve: TestCase[D] ⇒ Seq[Complex[D]]
  ): Results[D] = {
    val results =
      cases
        .map(Result(_))
        .toVector
        .sortBy(-_.maxErr)

    val grouped =
      results
        .runLengthPartial {
          case (cur, next)
            if (cur.maxErr == next.maxErr) ⇒
            cur
        }
        .toVector

    val sumErr = results.map(_.maxErr).sum
    val n = results.size
    val μ = sumErr / n
    val sumSqs = results.map(r ⇒ r.maxErr * r.maxErr).sum
    val σ = sqrt(sumSqs/n - μ*μ)
    Results(
      grouped,
      n,
      μ,
      σ
    )
  }

  implicit def showResults[D: Show : Field : Ordering](implicit sf: SigFigs): Show[Results[D]] =
    show {
      case Results(
        results,
        n,
        μ,
        σ
      ) ⇒
        (
          show"n $n μ $μ σ $σ:" ::
          results.take(3).map(_.show).toList
        )
        .mkString("\n", "\n\t", "\n")
    }
}

