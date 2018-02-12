package org.hammerlab.stats

import hammerlab.indent.implicits.tab
import hammerlab.lines._
import hammerlab.show._
import org.hammerlab.stats.Stats.fromHist
import org.hammerlab.Suite
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.format.SigFigs.showSigFigs
import spire.math.Integral

import scala.util.Random

/**
 * Tests of the [[Stats.fromHist]] API for constructing [[NonEmpty]] instances from "histograms" of elements that each come
 * with an associated repetition count, which allows the total number of elements represented to be much larger
 * ([[Long]] vs. [[Int]]).
 */
class HistShowTest extends Suite {

  Random.setSeed(123L)

  implicit val sf: SigFigs = 2

  def check[V: Integral : Show](input: Seq[(Int, V)],
                                expected: String): Unit =
    fromHist(input).showLines should be(expected.stripMargin)

  def check[V: Integral : Show](input: Seq[(Int, V)],
                                numToSample: Int,
                                expected: String): Unit =
    fromHist(
      input,
      numToSample
    )
    .showLines should be(
      expected.stripMargin
    )

  def check[V: Integral : Show](input: Seq[(Int, V)],
                                numToSample: Int,
                                onlySampleSorted: Boolean,
                                expected: String): Unit =
    fromHist(
      input,
      numToSample,
      onlySampleSorted
    )
    .showLines should be(
      expected.stripMargin
    )

  test("empty") {
    check(
      List[(Int, Int)](),
      "(empty)"
    )
  }

  test("single") {
    check(
      List(0 → 1),
      """N: 1, μ/σ: 0/0
        | elems: 0"""
    )
  }

  test("double") {
    check(
      List(0 → 2),
      """N: 2, μ/σ: 0/0
        | elems: 0×2"""
    )
  }

  test("two singles") {
    check(
      List(0 → 1, 1 → 1),
      """N: 2, μ/σ: 0.5/0.5
        | elems: 0 1"""
    )
  }

  test("three singles") {
    check(
      List(0 → 1, 5 → 1, 1 → 1),
      """N: 3, μ/σ: 2/2.2, med/mad: 1/1
        | elems: 0 5 1
        |sorted: 0 1 5"""
    )
  }

  test("single double") {
    check(
      List(0 → 1, 1 → 2),
      """N: 3, μ/σ: 0.67/0.47, med/mad: 1/0
        | elems: 0 1×2"""
    )
  }

  test("1×5 2×4") {
    check(
      List(1 → 5, 2 → 4),
      """N: 9, μ/σ: 1.4/0.5, med/mad: 1/0
        | elems: 1×5 2×4
        |  .50:	1
        |  .75:	2"""
    )
  }

  test("0×5 1×5") {
    check(
      List(0 → 5, 1 → 5),
      """N: 10, μ/σ: 0.5/0.5, med/mad: 0.5/0.5
        | elems: 0×5 1×5
        |  .25:	0
        |  .50:	0.5
        |  .75:	1"""
    )
  }

  test("0×4 1×6") {
    check(
      List(0 → 4, 1 → 6),
      """N: 10, μ/σ: 0.6/0.49, med/mad: 1/0
        | elems: 0×4 1×6
        |  .25:	0
        |  .50:	1"""
    )
  }

  test("x(x) 1 to 10") {
    check(
      (1 to 10).map(i ⇒ i → i),
      """N: 55, μ/σ: 7/2.4, med/mad: 7/2
        | elems: 1 2×2 3×3 4×4 5×5 6×6 7×7 8×8 9×9 10×10
        |  .05:	2
        |  .10:	3
        |  .25:	5
        |  .50:	7
        |  .75:	9
        |  .90:	10"""
    )
  }

  test("singletons") {
    check(
      (0 to 10).map(i ⇒ i → 1),
      """N: 11, μ/σ: 5/3.2, med/mad: 5/3
        | elems: 0 1 2 3 4 5 6 7 8 9 10
        |  .10:	0.2
        |  .25:	2
        |  .50:	5
        |  .75:	8
        |  .90:	9.8"""
    )
  }

  test("re-encode") {
    check(
      List(0 → 1, 0 → 1, 10 → 3, 10 → 4, 3 → 5, 0 → 2, 3 → 1),
      """N: 17, μ/σ: 5.2/4.2, med/mad: 3/3
        | elems: 0×2 10×7 3×5 0×2 3
        |sorted: 0×4 3×6 10×7
        |  .10:	0
        |  .25:	1.5
        |  .50:	3
        |  .75:	10"""
    )
  }

  test("large hist") {
    check(
      List(
        1 →  10000000000L,
        2 →   1000000000L,
        1 →          100L,
        2 →   1000000000L
      ),
      """N: 12000000100, μ/σ: 1.2/0.37, med/mad: 1/0
        | elems: 1×10000000000 2×1000000000 1×100 2×1000000000
        |sorted: 1×10000000100 2×2000000000
        |  .75:	1
        |  .90:	2"""
    )
  }
}
