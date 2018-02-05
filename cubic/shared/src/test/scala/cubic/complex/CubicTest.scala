package cubic.complex

import cats.Show
import cats.Show.show
import cats.implicits.catsStdShowForInt
import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.format.SigFigs.showSigFigs
import org.hammerlab.math.polynomial.PolySolverTest
import org.hammerlab.math.syntax.E
import org.hammerlab.test.CanEq
import spire.implicits.DoubleAlgebra
import spire.math.Complex

import scala.math.{ max, sqrt }

abstract class CubicTest
  extends PolySolverTest(3) {

  implicit var sigfigs: SigFigs = 3

  implicit val showComplex: Show[Complex[D]] =
    show {
      case Complex(r, i) ⇒
        if (i == 0)
          show"$r"
        else if (i < 0)
          show"$r${i}i"
        else
          show"$r+${i}i"
    }

  override val casePrintInterval: Int = 500

  implicit val showResult: Show[(Result, Int)] =
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

  def worstN(cases: Iterator[TestCase[D]], n: Int): Vector[(Result, Int)] =
    cases
      .map(Result(_))
      .toVector
      .sortBy(-_.maxErr)
      .runLengthEncode(
        (cur, next) ⇒
          if (cur.maxErr == next.maxErr)
            Some(cur)
          else
            None
      )
      .take(n)
      .toVector

  case class Result(tc: TestCase[D],
                    actual: Seq[Complex[D]],
                    maxErr: D)

  object Result {
    def apply(t: TestCase[D]): Result = {
      val TestCase(_, _, roots, Seq(a, b, c, d)) = t
      val actual: Seq[Complex[D]] = Cubic.doubleComplex.apply(a, b, c, d)

      if (roots.size != actual.size)
        throw new IllegalArgumentException(
          s"Sizes differ: $actual vs $roots"
        )

      val (aligned, maxErr) =
        actual
          .permutations
          .map {
            r ⇒
              r →
                roots
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
  }

  case class Results(results: Seq[(Result, Int)], n: Int, μ: D, σ: D) {
    def max = results.head._1.maxErr
  }

  object Results {
    def apply(cases: Iterator[TestCase[D]]): Results = {
      val results =
        cases
          .map(Result(_))
          .toVector
          .sortBy(-_.maxErr)

      val grouped =
        results
          .runLengthEncode(
            (cur, next) ⇒
              if (cur.maxErr == next.maxErr)
                Some(cur)
              else
                None
          )
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

    implicit val showResults: Show[Results] =
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

  case class FuzzyDouble(v: D, r: D)
  implicit class FuzzyDoubleOps(l: D) {
    def ±(r: D): FuzzyDouble = FuzzyDouble(l, r)
  }

  implicit val cmpFuzzyDouble =
    CanEq.instance[Double, FuzzyDouble, String] {
      case (l, FuzzyDouble(v, r)) ⇒
        implicit val ε: E = r
        doubleCmp.cmp(l, v)
    }

  def check(cases: Iterator[TestCase[D]],
                n: Int,
              max: D,
                μ: D,
                σ: D
           ): Unit = {
    val results = Results(cases)
    println(results.show)

    ===(results.  n, n)

    implicit val ε: E = 1e-3
    ===(results.max, max)
    ===(results.  μ,   μ)
    ===(results.  σ,   σ)
  }

  val M: Int = 6
}
