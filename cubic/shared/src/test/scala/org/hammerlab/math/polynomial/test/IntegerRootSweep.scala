package org.hammerlab.math.polynomial.test

import hammerlab.indent.implicits.spaces4
import hammerlab.iterator._
import hammerlab.show._
import org.hammerlab.math.polynomial.result.Stats
import org.hammerlab.math.polynomial.roots.RootShapes
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, Real, TestCase }

trait IntegerRootSweep[T] {

  self: PolySolverTest[T] ⇒

  val M: Int

  /**
   * Check all polynomials whose roots are comprised of integers in the range [-M, M] (via [[rootSweep]])
   *
   * Checked polynomials are partitioned into test-cases based on their [[RootShapes]] (multiplicities of real- and
   * imaginary-roots), because algorithm-code-paths (and corresponding floating-point-errors) tend to differ based on
   * them.
   *
   * @param elems Expected statistics about [[solve]]'s performance against ground-truth on [[TestCase]]s generated
   *              by sweeping roots over integers in the range [-[[M]], [[M]]] (via [[rootSweep]])
   */
  def sweepTests(implicit elems: (RootShapes, Stats)*): Unit =
    rootSweep
      .toList
      .groupBy {
        case TestCase(reals, imags, _, _) ⇒
          RootShapes(
            reals.map(_._2),
            imags.map(_._2)
          )
      }
      .toVector
      .sortBy(_._1)
      .foreach {
        case (shapes, cases) ⇒
          check(
            show"integer-roots sweep (max $M)",
            shapes,
            cases
          )
      }

  lazy val rootSweepSize =
    (
      for {
        numPairs ← 0 to N/2
        numReals = N - 2*numPairs
      } yield
        numRoots(numPairs, numImagPairs) * numRoots(numReals, 2*M + 1)
    )
    .sum

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
    for {
      numImaginaryRootPairs ← (0 to N/2).iterator
      numImaginaryRoots = 2 * numImaginaryRootPairs

      numRealRoots = N - numImaginaryRoots
      reals ← realRootsIter(numRealRoots)

      imags ← imaginaryRootPairs(numImaginaryRootPairs)
    } yield
      TestCase[D](
        reals,
        imags,
        1
      )

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
}
