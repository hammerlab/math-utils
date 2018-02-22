package org.hammerlab.math.polynomial.test

import hammerlab.indent.implicits.spaces4
import hammerlab.iterator._
import hammerlab.lines._
import hammerlab.show._
import org.hammerlab.math.polynomial.result.Stats
import org.hammerlab.math.polynomial.roots.RootShapes
import org.hammerlab.math.polynomial.{ Real, TestCase }
import org.hammerlab.math.syntax.E
import org.scalatest.exceptions.TestFailedException

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
  def sweepTests(elems: (RootShapes, Stats)*): Unit = {
    val expecteds = elems.toMap
    val cases = rootSweep.toList
    val n = cases.size
    cases
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
          val expected =
            expecteds
              .getOrElse(
                shapes,
                throw new Exception(
                  show"Missing expected stats for shapes: $shapes"
                )
              )

          test(show"integer-roots sweep (max $M): root-shapes: ${"%-7s".format(shapes.show)} expected: $expected") {
            val results: Results = cases.iterator
            val actual: Expected = results

            def print(): Unit = {
              println(
                show"$shapes:\t$actual"
              )
            }

            def err(e: Exception): Unit = {
              import hammerlab.lines.generic._
              val lines = actual.errors.lines
              val msg = lines.show
              print()
              println(indent(lines).show)
              throw new Exception(msg, e)
            }

            try {
              implicit val ε: E = 1e-2
              ===(actual.errors, expected)
            } catch {
              case e: TestFailedException ⇒ err(e)
            }
          }
      }
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
}
