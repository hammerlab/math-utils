package org.hammerlab.math.polynomial.result

import hammerlab.show._
import hammerlab.print.Limit
import hammerlab.lines._
import spire.algebra.Field

case class Zeros(expected: Int = 0, actual: Int = 0) {
  def isEmpty: Boolean = expected == 0 && actual == 0
  def nonEmpty: Boolean = !isEmpty
}
object Zeros {
  implicit val show: Show[Zeros] =
    Show {
      case Zeros(expected, actual) ⇒
        show"$expected,$actual mismatched zeros"
    }
}

case class Expected[D](abs: Stats,
                       ratio: Stats,
                       worst: Seq[ResultGroup[D]],
                       zeros: Zeros = Zeros())

object Expected {
  implicit def fromResults[D](results: Results[D])(implicit limit: Limit): Expected[D] =
    results match {
      case Results(
        results,
        absStats,
        ratioStats,
        zeros
      ) ⇒
        Expected(
          absStats,
          ratioStats,
          results.take(limit.size.getOrElse(0)),
          zeros
        )
    }

/*
  implicit def show[D: Show : Field : Ordering](implicit s: Show[Double]): ToLines[Expected[D]] =
    ToLines {
      case Expected(
        abs,
        ratio,
        worst,
        zeros
      ) ⇒
        val mismatchedZeros =
          if (zeros.nonEmpty)
            show" ($zeros)"
          else
            ""

        Lines(
          show"abs: $abs ratios: $ratio$mismatchedZeros",
          indent(
            worst
          )
        )
    }
*/
}
