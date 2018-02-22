package org.hammerlab.math.polynomial.test

import hammerlab.indent.implicits.spaces4
import hammerlab.iterator._
import org.hammerlab.math.polynomial.result.Solve
import hammerlab.math.FromDouble
import hammerlab.show._
import org.hammerlab.Suite
import org.hammerlab.io.print.Limit
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.polynomial
import org.hammerlab.math.polynomial.result.{ Expected, Results, Stats }
import org.hammerlab.math.polynomial.roots.RootShapes
import org.hammerlab.math.polynomial.roots.dsl.IsRootShapes
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, Real, TestCase, result }
import org.hammerlab.math.syntax.Doubleish._
import org.hammerlab.math.syntax.{ Doubleish, E }
import org.hammerlab.test.CanEq
import org.scalatest.exceptions.TestFailedException
import shapeless.HNil
import spire.algebra.{ Field, IsReal, NRoot, Signed, Trig }
import spire.math.{ Complex, abs }

import scala.math.exp
import scala.util.Random._

trait ShowD[D] {
  /**
   * For pretty-printing purposes, display [[D]]s as corresponding [[Double]] values
   */
  implicit def showD(implicit db: Doubleish[D], sd: Show[Double]): Show[D] =
    Show { _.toDouble.show }
}

abstract class PolySolverTest[T : FromDouble : IsReal : NRoot : Signed : Trig](degree: Int)(
    implicit
    // name and `val` these bc mix-ins need them
    val field: Field[T],
    val ordering: Ordering[T],
    val doubleish: Doubleish[T]
)
  extends Suite
    with ShowD[T]
    with IntegerRootSweep[T]
    with IsRootShapes.HasOps {

  /** Pretty-printing level */
  implicit var sigfigs: SigFigs = 3

  implicit def showDouble: Show[Double] = org.hammerlab.math.format.SigFigs.showSigFigs

  type D = T

  val N = degree

  /**
   * Subclasses implement solving for roots of a [[TestCase]] here
   */
  implicit def solve(t: TestCase[D]): Seq[Complex[D]]
  implicit def solver: Solve[D] = solve

  /**
   * Expected statistics about [[solve]]'s performance against ground-truth on [[TestCase]]s with randomly-generated
   * roots (via [[randomCases]])
   */
  def random: Expected
  test("uniform random roots") {
    check(
      randomCases(gaussian),
      random
    )
  }

  def logNormalRandom: Expected
  test("log-normal random roots") {
    check(
      randomCases(logNormal),
      logNormalRandom
    )
  }

  /**
   * Convenience-constructor for [[Expected]] instances containing expected values
   */
  def expected(errors: Stats,
               worst: ResultGroup*): Expected =
    Expected(
      errors,
      worst
    )

  implicit val limit: Limit = 3

  type Real    = polynomial.Real[D]
  type Result = result.Result[D]
  type Results = result.Results[D]
  type ResultGroup = result.ResultGroup[D]
  type Expected = result.Expected[D]

  /**
   * Run a polynomial-solver on some [[TestCase]]s, compute [[Results]] statistics about the solver's error relative to
   * the true roots, and verify them against provided/expected values
   *
   * @param cases test-cases to run
   */
  def check(cases: Iterator[TestCase[D]],
            expected: Expected): Unit = {
    val results = Results(cases)
    val actual: Expected = results

/*
    println(
      Lines(
        show"Error statistics: ${actual.errors}",
        s"Worst cases:",
        indent(
          actual.worst.lines
        )
      )
      .showLines
    )
*/

    {
      /** check (and output) error-statistics to 2-3 digits' accuracy */
      implicit val ε: E = 1e-2

      /**
       * skip comparing the [[result.Expected.worst]] entries, since tests aren't populating "expected" values for
       * them atm
       */
      try {
        ===(actual, expected)
      } catch {
        case e: TestFailedException ⇒
          println(actual.errors.show)
          throw e
      }
    }
  }

  implicit val resultsCanEqExpected: CanEq[Results, Expected] =
    new CanEq[Results, Expected] {
      import org.hammerlab.test.CanEq.Cmp
      val cmpStats = shapeless.the[Cmp[Stats]]
      override type Error = cmpStats.Error
      override def cmp(l: Results, r: Expected): Option[Error] = {
        cmpStats(l.errors, r.errors)
      }
    }

  /** Helpers for converting to [[D]] */
  val fromD = FromDouble[D] _
  implicit val fromInt   : Int    ⇒ D = (x: Int) ⇒ fromD(x)
  implicit val fromDouble: Double ⇒ D = fromD

  /**
   * All distinct imaginary-root pairs with integral coefficients in the range [-M,M]
   */
  lazy val allImaginaryRootPairs =
    for {
      a ← -M to M
      b ←  1 to M
    } yield
      ImaginaryRootPair[D](a, b)

  val C = hammerlab.math.binomial

  def numRoots(roots: Int, options: Int) = C(options + roots - 1, roots)

  lazy val numImagPairs = allImaginaryRootPairs.size

  def gaussian(): D = nextGaussian()

  def logNormal(): D =
    exp(
      nextGaussian()
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
  def printEveryN(it: Iterator[TestCase[T]]): Iterator[TestCase[T]] =
    it
      .zipWithIndex
      .map {
        case (d, idx) ⇒
          if (idx % casePrintInterval == 0)
            println(show"iteration $idx:\t$d")
          d
      }

  /**
   * Test this many random roots-values for each overall "shape" (number and multiplicity of real and imaginary roots)
   */
  def iterationsPerRootShape: Int

  /**
   * Generate [[iterationsPerRootShape]] [[TestCase]]s – root-sets of polynomials of degree [[N]], along with derived
   * coefficients – for each possible roots-"shape" ({number of imaginary-root-pairs} X {multiplicity of real and
   * imaginary roots}).
   *
   * The free variables (values of real roots, and real and imaginary parts for imaginary-root-pairs, and a "scale" to
   * multiply all coefficients by) sampled from a standard normal distribution.
   */
  def randomCases(rnd: () => D): Iterator[TestCase[D]] = {
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

//        scale ← (1 to scalesPerIteration).map(_ ⇒ rnd())
        scale = rnd()
      } yield
        TestCase(
          reals,
          imags,
          scale
        )
    )
  }

  implicit def convTuple[L, R](t: (L, R))(implicit f: IsRootShapes[L]): (RootShapes, R) = (f(t._1), t._2)

  implicit def makeResults(cases: Iterator[TestCase[D]]): Results = Results(cases)
}
