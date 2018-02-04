package org.hammerlab.math.polynomial

import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.Suite
import org.hammerlab.math.format.showSuperscript
import org.hammerlab.math.syntax.Doubleish
import spire.algebra.Ring
import spire.implicits.{IntAlgebra, DoubleAlgebra}
import spire.syntax.all._
import spire.math.Complex

import math.abs
import scala.Array.fill

abstract class PolySolverTest(N: Int)
  extends Suite {

  type Dbl = Double

  type R[T]

  type Real[T] <: R[T]

  def root[T](value: T, degree: Int): Seq[Real[T]]

  def M: Int

  val casePrintInterval = 100

  val allImaginaryRootPairs =
    for {
      a ← -M to M
      b ←  1 to M
    } yield
      ImaginaryRootPair[Dbl](a, b)

  def toComplex[T: Ring](r: R[T]): Seq[Complex[T]]

  implicit class RootOps[T](r: R[T]) {
    def complex(implicit e: Ring[T]): Seq[Complex[T]] = toComplex(r)
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
      s"rts: ${(reals ++ imags.map(_.show)).mkString(", ")}\t coeffs: ${coeffs.mkString(" ")}"
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
  def realRootsIter(num: Int): Iterator[List[R[Dbl]]] =
    (-M to M)
      .unorderedSubsetsWithReplacement(num)
      .map {
        _.flatMap {
          case (value, arity) ⇒
            root[Dbl](value, arity)
        }
      }

  /**
   * Generate roots and corresponding coefficients for all possible mixtures of real/imaginary roots with coefficients
   * in [-M,m]
   */
  def rootSweep: Iterator[TestCase[Dbl]] =
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
          coeffs[Dbl](roots, scale)
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
            println(s"iteration $idx:\t$d")
          d
      }

  import Doubleish.DoubleishOps

  def randomCases(implicit d: Doubleish[Real[Dbl]]): Iterator[TestCase[Dbl]] = {
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

        reals = realArities.flatMap(root(rnd(), _)).sortBy(_.toDouble)

        imags = imagPairArities.map(ImaginaryRootPair(rnd(), abs(rnd())) → _)

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
          coeffs[Dbl](roots, scale)
        )
    )
  }
}
