package org.hammerlab.math.polynomial

import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.Suite
import org.hammerlab.math.format.showSuperscript
import spire.algebra.{ Ring, Semiring }
import spire.implicits._
import spire.math.Complex

import scala.Array.fill

abstract class PolySolverTest(N: Int)
  extends Suite {

  type Dbl = scala.Double

  type R[T] <: Root[T]

  def root[T: Ring](value: T, degree: Int): R[T]

  def M: Int

  val casePrintInterval = 100

  val allImaginaryRootPairs =
    for {
      a ← -M to M
      b ←  1 to M
    } yield
      ImaginaryRootPair(a, b)

  /**
   * A pair of conjugate imaginary numbers, [[a]]±[[b]]i, both of which are roots of a quartic polynomial
   *
   * We're testing polynomials with real coefficients, so imaginary roots must come in conjugate-pairs
   *
   * [[b]] must be greater than zero; real roots are not modeled with this data structure
   */
  case class ImaginaryRootPair[T](a: T, b: T) {
    override def toString: String =
      (if (a == 0) "" else s"$a") +
      "±" +
      (if (b == 1) "" else s"$b") +
      "i"
  }

  implicit class ImaginaryRootPairOps[T](r: ImaginaryRootPair[T]) {
    def complex(implicit e: Ring[T]): Seq[Complex[T]] =
      Seq(
        Complex(r.a, -r.b),
        Complex(r.a,  r.b)
      )
  }

  implicit class RootOps[T](r: R[T]) {
    def complex(implicit e: Semiring[T]): Array[Complex[T]] =
      fill(
        r.degree
      )(
        Complex(r.value)
      )
  }

  import cats.Show
  import Show.show

  /**
   * Pretty-print an imaginary-conjugate root-pair, possibly repeated as a multiple root (denoted by a superscript, as
   * with [[Root]])
   */
  implicit def showImagRoot[T]: Show[(ImaginaryRootPair[T], Int)] =
    show {
      case (pair, 1) ⇒ pair.toString
      case (pair, degree) ⇒ s"($pair)${showSuperscript.show(degree)}"
    }

  /**
   * Iterate over the available imaginary-root conjugate-pairs (with integral real and imaginary parts in [-M, M], per
   * [[allImaginaryRootPairs]]), emitting all possible multi-sets of size `num`, and allowing repeats of individual
   * pairs (representing multiple roots)
   */
  def imaginaryRootPairs(num: Int) =
    allImaginaryRootPairs
      .unorderedSubsetsWithReplacement(num)

  case class TestCase[T]( reals: Seq[R[T]],
                          imags: Seq[(ImaginaryRootPair[T], Int)],
                          roots: Seq[Complex[T]],
                         coeffs: Seq[T]) {
    override def toString: String =
      s"rts: ${(reals ++ imags.map(_.show)).mkString(" ")}\t coeffs: ${coeffs.mkString(" ")}"
  }

  object TestCase {
    def apply[T: Ring](reals: Seq[R[T]],
                       imags: Seq[(ImaginaryRootPair[T], Int)],
                       scale: T): TestCase[T] = {
      val roots =
        reals
          .flatMap(_.complex) ++
        imags
          .flatMap { case (ab, d) ⇒ fill(d)(ab) }
          .flatMap(_.complex)

      TestCase(
        reals,
        imags,
        roots,
        coeffs(roots, scale)
      )
    }
  }

  /**
   * Iterator over all sets of `num` integer-roots in the range [-M,M] (including double/triple/quadruple roots)
   */
  def realRootsIter(num: Int): Iterator[List[R[Int]]] =
    (-M to M)
      .unorderedSubsetsWithReplacement(num)
      .map {
        _.map {
          case (value, arity) ⇒
            root(value, arity)
        }
      }

  /**
   * Generate roots and corresponding coefficients for all possible mixtures of real/imaginary roots with coefficients
   * in [-M,m]
   */
  def rootSweep: Iterator[TestCase[Int]] =
    printEveryN(
      for {
        numImaginaryRootPairs ← (0 to N/2).iterator
        numImaginaryRoots = 2 * numImaginaryRootPairs

        numRealRoots = N - numImaginaryRoots
        reals ← realRootsIter(numRealRoots)

        imags ← imaginaryRootPairs(numImaginaryRootPairs)

        /** Expand multiple roots, convert to [[Complex]]s */
        roots =
          reals
            .flatMap(_.complex) ++
          imags
            .flatMap { case (ab, d) ⇒ fill(d)(ab) }
            .flatMap(_.complex)

        scale ← -scalesPerIteration to scalesPerIteration
        if scale != 0

      } yield
        TestCase(
          reals,
          imags,
          roots,
          coeffs(roots, scale)
        )
    )

  import scala.util.Random._
  def rnd(): Double = {
    nextGaussian()
  }

  def coeffs[T: Ring](roots: Seq[Complex[T]], scale: T): Seq[T] = {
    /** Infer the coefficients of the quartic equation from the roots */
    scale ::
    (
      for {
        coeff ← 1 to N
      } yield
        roots
          .unorderedSubsets(coeff)
          .map(_.reduce(_ * _))
          .reduce(_ + _) *
            (
              if (coeff % 2 == 0)
                Ring[T].fromInt(1)
              else
                Ring[T].fromInt(-1)
            )
    )
    .map(_.real)  // coefficients will always be real because all imaginary roots come in conjugate-pairs
    .map(_ * scale)
    .toList
  }

  val iterationsPerRootShape = 100
  val scalesPerIteration = 3

  def printEveryN[T](it: Iterator[TestCase[T]]): Iterator[TestCase[T]] =
    it
      .zipWithIndex
      .map {
        case (d, idx) ⇒
          if (idx % casePrintInterval == 0)
            println(s"iteration $idx: $d")
          d
      }


  def randomCases: Iterator[TestCase[Double]] = {
    scala.util.Random.setSeed(123)
    printEveryN(
      for {
        numImagPairs ← (0 to N/2).iterator
        numImagRoots = 2 * numImagPairs
        imagPairArities ← numImagPairs.unorderedPartitions

        numRealRoots = N - numImagRoots
        realArities ← numRealRoots.unorderedPartitions

        // _ = println(s"real/imaginary root-arities: ${realArities.mkString(",")} ${imagPairArities.mkString(",")}")

        // do 10 reps with each possible arity-distribution of [imaginary X real] roots
        _ ← 1 to iterationsPerRootShape

        reals = realArities.map(root(rnd(), _)).sortBy(_.value)

        imags = imagPairArities.map(ImaginaryRootPair(rnd(), rnd()) → _)

        roots =
          reals
            .flatMap(_.complex) ++
          imags
            .flatMap { case (pair, arity) ⇒ fill(arity)(pair) }
            .flatMap(_.complex)

        scale ← (1 to scalesPerIteration).map(_ ⇒ rnd())
      } yield
        TestCase(
          reals,
          imags,
          roots,
          coeffs(roots, scale)
        )
    )
  }
}
