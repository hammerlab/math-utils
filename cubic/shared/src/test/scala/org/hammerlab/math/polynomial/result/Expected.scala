package org.hammerlab.math.polynomial.result

import hammerlab.lines.limit._
import hammerlab.show._
import spire.algebra.Field

case class Expected[D](errors: Stats,
                       worst: Seq[ResultGroup[D]])

object Expected {
  implicit def fromResults[D](results: Results[D])(implicit limit: Limit): Expected[D] =
    results match {
      case Results(
        results,
        errors
      ) ⇒
        Expected(
          errors,
          results.take(
            limit match {
              case Max(n) ⇒ n
              case _ ⇒ 0
            }
          )
        )
    }

  implicit def show[D: Show : Field : Ordering](implicit s: Show[Double]): Show[Expected[D]] =
    Show {
      case Expected(
        errors,
        worst
      ) ⇒
        show"$errors, ${worst.head}"
    }
}
