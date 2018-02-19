package org.hammerlab.math.polynomial.result

import hammerlab.show._
import spire.math.sqrt

import scala.math.max

case class Stats(n: Int,
                 μ: Double,
                 σ: Double,
                 max: Double)

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
    Show {
      case Stats(n, μ, σ, max) ⇒
        show"n $n μ $μ σ $σ max $max"
    }
}
