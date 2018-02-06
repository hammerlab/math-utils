package org.hammerlab.math.polynomial

import cats.implicits.catsStdShowForInt
import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.math.format.SigFigs.showSigFigs
import hammerlab.iterator._
import org.hammerlab.Suite
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.polynomial
import org.hammerlab.math.syntax.E
import spire.implicits._
import spire.math.Complex

import scala.math.{ abs, exp }
import scala.util.Random._

abstract class PolySolverTest(N: Int)
  extends Suite {

  type D = Double
  type Real[T] = polynomial.Real[T]
  type R[T] = polynomial.Root[T]

  def M: Int

  implicit var sigfigs: SigFigs = 3

  val casePrintInterval = 1000

  /**
   * All distinct imaginary-root pairs with integral coefficients in the range [-M,M]
   */
  val allImaginaryRootPairs =
    for {
      a ← -M to M
      b ←  1 to M
    } yield
      ImaginaryRootPair[D](a, b)

  /**
   * Iterate over the available imaginary-root conjugate-pairs (with integral real and imaginary parts in [-M, M], per
   * [[allImaginaryRootPairs]]), emitting all possible multi-sets of size `num`, and allowing repeats of individual
   * pairs (representing multiple roots)
   */
  def imaginaryRootPairs(num: Int) =
    allImaginaryRootPairs
      .unorderedSubsetsWithReplacement(num)

  /**
   * Iterator over all sets of `num` integer-roots in the range [-M,M] (including double/triple/quadruple roots)
   */
  def realRootsIter(num: Int): Iterator[List[(Real[D], Int)]] =
    (-M to M)
      .unorderedSubsetsWithReplacement(num)
      .map {
        _.map {
          case (value, arity) ⇒
            Real(value.toDouble) → arity
        }
      }

  /**
   * Generate roots and corresponding coefficients for all possible mixtures of real/imaginary roots with coefficients
   * in [-M,m]
   */
  def rootSweep: Iterator[TestCase[D]] =
    printEveryN(
      for {
        numImaginaryRootPairs ← (0 to N/2).iterator
        numImaginaryRoots = 2 * numImaginaryRootPairs

        numRealRoots = N - numImaginaryRoots
        reals ← realRootsIter(numRealRoots)

        imags ← imaginaryRootPairs(numImaginaryRootPairs)

        scale ← -scalesPerIteration to scalesPerIteration
        if scale != 0

      } yield
        TestCase[D](
          reals,
          imags,
          scale
        )
    )

  def rnd(): Double = nextGaussian()

  def logNormal(): Double =
    exp(
      nextGaussian() * 5
    ) *
    (
      if (nextBoolean())
        1
      else
        -1
    )

  val iterationsPerRootShape = 100
  val scalesPerIteration = 3

  def printEveryN[T](it: Iterator[TestCase[T]]): Iterator[TestCase[T]] =
    it
      .zipWithIndex
      .map {
        case (d, idx) ⇒
          if (idx % casePrintInterval == 0)
            println(s"iteration $idx:\t$d")
          d
      }

  def randomCases: Iterator[TestCase[D]] = {
    scala.util.Random.setSeed(123)
    printEveryN(
      for {
        numImagPairs ← (0 to N/2).iterator
        numImagRoots = 2 * numImagPairs
        imagPairArities ← numImagPairs.unorderedPartitions

        numRealRoots = N - numImagRoots
        realArities ← numRealRoots.unorderedPartitions

        // do 10 reps with each possible arity-distribution of [imaginary X real] roots
        _ ← 1 to iterationsPerRootShape

        reals = realArities.map(Real(rnd()) → _).sortBy(_._1.t)

        imags = imagPairArities.map(ImaginaryRootPair(rnd(), abs(rnd())) → _)

        scale ← (1 to scalesPerIteration).map(_ ⇒ rnd())
      } yield
        TestCase(
          reals,
          imags,
          scale
        )
    )
  }

  implicit def solve(t: TestCase[D]): Seq[Complex[D]]

  def check(cases: Iterator[TestCase[D]],
                n: Int,
              max: D,
                μ: D,
                σ: D
           ): Unit = {
    val results = Results(cases)
    println(results.show)

    ===(results.  n, n)

    {
      implicit val ε: E = 1e-3
      ===(results.max, max)
      ===(results.  μ,   μ)
      ===(results.  σ,   σ)
    }
  }
}
