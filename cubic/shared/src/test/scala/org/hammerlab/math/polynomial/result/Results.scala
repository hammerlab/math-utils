package org.hammerlab.math.polynomial.result

import hammerlab.iterator._
import org.hammerlab.math.polynomial.TestCase
import org.hammerlab.math.syntax.Doubleish
import spire.algebra.{ Field, IsReal, NRoot, Signed, Trig }

case class Results[D](results: Seq[ResultGroup[D]],
                      errors: Stats)

object Results {
  def apply[D: Field : Doubleish : NRoot : Signed : Solve](cases: Iterator[TestCase[D]]): Results[D] = {
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
        .map(t ⇒ t: ResultGroup[D])
        .toVector

    Results(
      grouped,
      Stats(
        results
          .flatMap(_.actual)
          .map(_.err)
      )
    )
  }
}

