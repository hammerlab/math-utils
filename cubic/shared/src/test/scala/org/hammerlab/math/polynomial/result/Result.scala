package org.hammerlab.math.polynomial.result

import hammerlab.lines._
import hammerlab.show._
import org.hammerlab.math.polynomial.TestCase
import org.hammerlab.math.polynomial.result.Result.Root
import org.hammerlab.math.syntax.Doubleish
import org.hammerlab.math.syntax.Doubleish._
import spire.algebra.{ Field, NRoot, Signed }
import spire.implicits._
import spire.math.Complex

case class Result[D](tc: TestCase[D],
                     actual: Seq[Root[D]],
                     maxErr: Double)

object Result {

  case class Root[D](expected: Complex[D],
                     actual: Complex[D],
                     err: Double)

  def apply[D: Field : Doubleish : Solve : NRoot : Signed](t: TestCase[D]): Result[D] = {
    val actual: Seq[Complex[D]] = Solve[D].apply(t)

    if (t.roots.size != actual.size)
      throw new IllegalArgumentException(
        s"Sizes differ: $actual vs ${t.roots}"
      )

    /* Measure computed roots' error of relative to the largest root-magnitude */
    val rootScale =
      t
        .roots
        .map(_.abs)
        .max

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
            val errors: Seq[Root[D]] =
              t
                .roots
                .zip(r)
                .map {
                  case (expected, actual) ⇒
                    val diff = (actual - expected).abs
                    Root(
                      expected,
                      actual,
                      if (diff.isZero)
                        0
                      else
                        (diff / rootScale).toDouble
                    )
                }

            (errors, errors.map(_.err).max)
        }
        .minBy(_._2)

    Result(
      t,
      aligned,
      maxErr
    )
  }

  import ResultGroup.showResultGroupOpt
  implicit def showResult[D: Show : Field : Ordering](implicit d: Show[Double], indent: Indent): ToLines[Result[D]] =
    ToLines {
      r ⇒ (r, None: Option[Int]).lines
    }
}
