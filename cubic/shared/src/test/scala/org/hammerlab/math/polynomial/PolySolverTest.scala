package org.hammerlab.math.polynomial

import cats.Show
import cats.Show.show
import cats.implicits.catsStdShowForInt
import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.math.format.SigFigs.showSigFigs
import hammerlab.iterator._
import org.hammerlab.Suite
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.polynomial
import org.hammerlab.math.syntax.{ Doubleish, E }
import spire.algebra.{ Field, IsReal, NRoot, Signed }
import spire.math.Complex
import spire.math.abs

import scala.math.exp
import scala.util.Random._

abstract class PolySolverTest[D : Ordering : Field : Doubleish : IsReal : NRoot : Signed](N: Int)
  extends Suite {

  /** Helpers for converting to [[D]] */
  val field = Field[D]
  implicit val fromInt    = field.fromInt _
  implicit val fromDouble = field.fromDouble _

  implicit val showD: Show[D] =
    show {
      d ⇒
        import Doubleish._
        Show[Double].show(d.toDouble)
    }

  type Real = polynomial.Real[D]

  /** Check all polynomials whose roots are comprised of integers in the range [-[[M]], [[M]]], via [[rootSweep]] */
  def M: Int

  /** Pretty-printing level */
  implicit var sigfigs: SigFigs = 3

  /**
   * Subclasses implement solving for roots of a [[TestCase]] here
   */
  implicit def solve(t: TestCase[D]): Seq[Complex[D]]

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
  def realRootsIter(num: Int): Iterator[List[(Real, Int)]] =
    (-M to M)
      .unorderedSubsetsWithReplacement(num)
      .map {
        _.map {
          case (value, arity) ⇒
            Real(value: D) → arity
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

  def rnd(): D = nextGaussian()

  def logNormal(): D =
    exp(
      nextGaussian() * 5
    ) *
    (
      if (nextBoolean())
         1
      else
        -1
    )

  val casePrintInterval = 1000

  /**
   * Helper for printing status messages while potentially brute-forcing many polynomial-solving iterations, e.g. via
   * [[randomCases]] or [[rootSweep]]
   */
  def printEveryN[T](it: Iterator[TestCase[T]]): Iterator[TestCase[T]] =
    it
      .zipWithIndex
      .map {
        case (d, idx) ⇒
          if (idx % casePrintInterval == 0)
            println(s"iteration $idx:\t$d")
          d
      }

  /**
   * Test this many random roots-values for each overall "shape" (number and multiplicity of real and imaginary roots)
   */
  val iterationsPerRootShape = 100

  /**
   * For each random-root iteration in [[randomCases]] (with a fixed set of values for all roots), perform
   * [[scalesPerIteration]] tests, each time scaling all coefficients by a random factor (which doesn't change the
   * underlying roots).
   *
   * This "scale" (coefficient of the highest-exponent term in the polynomial) is divided out of all coefficients as a
   * first step, leaving a monic polynomial, but this helps explore overall stability of the solvers, and their
   * sensitivity to floating-point error.
   */
  val scalesPerIteration = 3

  /**
   * Generate [[iterationsPerRootShape]] [[TestCase]]s – root-sets of polynomials of degree [[N]], along with derived
   * coefficients – for each possible roots-"shape" ({number of imaginary-root-pairs} X {multiplicity of real and
   * imaginary roots}).
   *
   * The free variables (values of real roots, and real and imaginary parts for imaginary-root-pairs, and a "scale" to
   * multiply all coefficients by) sampled from a standard normal distribution.
   */
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

        reals = realArities.map(Real(rnd()) → _)

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

  /**
   * Run a polynomial-solver on some [[TestCase]]s, compute [[Results]] statistics about the solver's error relative to
   * the true roots, and verify them against provided/expected values
   *
   * @param cases test-cases to run
   * @param n expected number of cases
   * @param max maximum error found on any [[TestCase]] in `cases`
   * @param μ expected mean error
   * @param σ expected error stddev
   */
  def check(cases: Iterator[TestCase[D]],
                n: Int,
              max: Double,
                μ: Double,
                σ: Double
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
