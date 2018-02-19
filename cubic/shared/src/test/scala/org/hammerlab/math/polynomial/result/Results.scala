package org.hammerlab.math.polynomial.result

import hammerlab.iterator._
import hammerlab.monoid._
import hammerlab.show._
import org.hammerlab.math.polynomial.{ TestCase, result }
import org.hammerlab.math.syntax.{ Doubleish, E }
import spire.algebra.{ Field, IsReal, NRoot, Trig }
import spire.math.Complex

case class Results[D](results: Seq[ResultGroup[D]],
                      absStats: Stats,
                      ratioStats: Stats,
                      zeros: Zeros)

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
        .map(t ⇒ t: ResultGroup[D])
        .toVector

    Results(
      grouped,
      Stats(results.    map(_.maxAbsErr)),
      result.Stats(results.flatMap(_.maxErrRatio)),
      results.map(_.zeros).reduceLeft(_ |+| _)
    )
  }
}

