package org.hammerlab.stats

import hammerlab.show._
import org.hammerlab.test.Suite
import spire.math.Numeric

import scala.util.Random.{ nextInt, setSeed, shuffle }

/**
 * Test the default [[cats.Show.show show]] method of [[Stats]] instances.
 */
class ShowTest extends Suite {

  setSeed(123L)

  def check[K : Numeric : Ordering : Show](input: Seq[K], expected: String): Unit =
    Stats(input).show should be(
      expected.stripMargin
    )

  def check[K : Numeric : Ordering : Show](input: Seq[K],
                                           numToSample: Int,
                                           expected: String): Unit =
    Stats(
      input,
      numToSample
    )
    .show should be(
      expected.stripMargin
    )

  def check[K : Numeric : Ordering : Show](input: Seq[K],
                                           numToSample: Int,
                                           onlySampleSorted: Boolean,
                                           expected: String): Unit =
    Stats(
      input,
      numToSample,
      onlySampleSorted
    )
    .show should be(
      expected.stripMargin
    )

  test("empty") {
    check[Int](
      Nil,
      "(empty)"
    )
  }

  test("0 to 0") {
    check(
      0 to 0,
      """N: 1, μ/σ: 0/0
        | elems: 0"""
    )
  }

  test("0 to 1") {
    check(
      0 to 1,
      """N: 2, μ/σ: 0.5/0.5
        | elems: 0 1"""
    )
  }

  test("1 to 0") {
    check(
      1 to 0 by -1,
      """N: 2, μ/σ: 0.5/0.5
        | elems: 1 0
        |sorted: 0 1"""
    )
  }

  test("0 to 2") {
    check(
      0 to 2,
      """N: 3, μ/σ: 1/0.8, med/mad: 1/1
        | elems: 0 1 2"""
    )
  }

  test("2 to 0") {
    check(
      2 to 0 by -1,
      """N: 3, μ/σ: 1/0.8, med/mad: 1/1
        | elems: 2 1 0
        |sorted: 0 1 2"""
    )
  }

  test("0 to 3") {
    check(
      0 to 3,
      """N: 4, μ/σ: 1.5/1.1, med/mad: 1.5/1
        | elems: 0 1 2 3"""
    )
  }

  test("3 to 0") {
    check(
      3 to 0 by -1,
      """N: 4, μ/σ: 1.5/1.1, med/mad: 1.5/1
        | elems: 3 2 1 0
        |sorted: 0 1 2 3"""
    )
  }

  test("1 to 9") {
    check(
      1 to 9,
      """N: 9, μ/σ: 5/2.6, med/mad: 5/2
        | elems: 1 2 3 4 5 6 7 8 9
        |  10:	1
        |  25:	2.5
        |  50:	5
        |  75:	7.5
        |  90:	9"""
    )
  }

  test("9 to 1") {
    check(
      9 to 1 by -1,
      """N: 9, μ/σ: 5/2.6, med/mad: 5/2
        | elems: 9 8 7 6 5 4 3 2 1
        |sorted: 1 2 3 4 5 6 7 8 9
        |  10:	1
        |  25:	2.5
        |  50:	5
        |  75:	7.5
        |  90:	9"""
    )
  }

  val shuffled1to9 = shuffle(1 to 9).toArray

  test("1 to 9 sample 5") {
    check(
      shuffled1to9,
      numToSample = 5,
      """N: 9, μ/σ: 5/2.6, med/mad: 5/2
        | elems: 8 4 5 3 1 6 7 2 9
        |sorted: 1 2 3 4 5 6 7 8 9
        |  10:	1
        |  25:	2.5
        |  50:	5
        |  75:	7.5
        |  90:	9"""
    )
  }

  test("1 to 9 sample 4") {
    check(
      shuffled1to9,
      numToSample = 4,
      """N: 9, μ/σ: 5/2.6, med/mad: 5/2
        | elems: 8 4 5 3 … 6 7 2 9
        |sorted: 1 2 3 4 … 6 7 8 9
        |  10:	1
        |  25:	2.5
        |  50:	5
        |  75:	7.5
        |  90:	9"""
    )
  }

  test("1 to 9 sample 3") {
    check(
      shuffled1to9,
      numToSample = 3,
      """N: 9, μ/σ: 5/2.6, med/mad: 5/2
        | elems: 8 4 5 … 7 2 9
        |sorted: 1 2 3 … 7 8 9
        |  10:	1
        |  25:	2.5
        |  50:	5
        |  75:	7.5
        |  90:	9"""
    )
  }

  test("1 to 9 sample 2") {
    check(
      shuffled1to9,
      numToSample = 2,
      """N: 9, μ/σ: 5/2.6, med/mad: 5/2
        | elems: 8 4 … 2 9
        |sorted: 1 2 … 8 9
        |  10:	1
        |  25:	2.5
        |  50:	5
        |  75:	7.5
        |  90:	9"""
    )
  }

  test("1 to 9 sample 1") {
    check(
      shuffled1to9,
      numToSample = 1,
      """N: 9, μ/σ: 5/2.6, med/mad: 5/2
        | elems: 8 … 9
        |sorted: 1 … 9
        |  10:	1
        |  25:	2.5
        |  50:	5
        |  75:	7.5
        |  90:	9"""
    )
  }

  test("1 to 9 sample 0") {
    check(
      shuffled1to9,
      numToSample = 0,
      """N: 9, μ/σ: 5/2.6, med/mad: 5/2
        |  10:	1
        |  25:	2.5
        |  50:	5
        |  75:	7.5
        |  90:	9"""
    )
  }

  test("1 to 99") {
    check(
      1 to 99,
      """N: 99, μ/σ: 50/28.6, med/mad: 50/25
        | elems: 1 2 3 4 5 6 7 8 9 10 … 90 91 92 93 94 95 96 97 98 99
        |   1:	1
        |   5:	5
        |  10:	10
        |  25:	25
        |  50:	50
        |  75:	75
        |  90:	90
        |  95:	95
        |  99:	99"""
    )
  }

  test("99 to 1") {
    check(
      99 to 1 by -1,
      """N: 99, μ/σ: 50/28.6, med/mad: 50/25
        | elems: 99 98 97 96 95 94 93 92 91 90 … 10 9 8 7 6 5 4 3 2 1
        |sorted: 1 2 3 4 5 6 7 8 9 10 … 90 91 92 93 94 95 96 97 98 99
        |   1:	1
        |   5:	5
        |  10:	10
        |  25:	25
        |  50:	50
        |  75:	75
        |  90:	90
        |  95:	95
        |  99:	99"""
    )
  }

  val shuffledDigits = (0 until 100).map(_ ⇒ nextInt(10))

  test("100 digits") {
    check(
      shuffledDigits,
      """N: 100, μ/σ: 4.3/2.9, med/mad: 4.5/2.5
        | elems: 5 3 9 6 2 5 7 9 0 5 … 7 9 1 9 0×2 8 0 7×2 0 6
        |sorted: 0×15 1×7 2×8 3×11 4×9 5×12 6×11 7×9 8×9 9×9
        |   1:	0
        |   5:	0
        |  10:	0
        |  25:	2
        |  50:	4.5
        |  75:	7
        |  90:	8
        |  95:	9
        |  99:	9"""
    )
  }

  test("100 digits sample 4") {
    check(
      shuffledDigits,
      numToSample = 4,
      """N: 100, μ/σ: 4.3/2.9, med/mad: 4.5/2.5
        | elems: 5 3 9 6 … 0 7×2 0 6
        |sorted: 0×15 1×7 2×8 3×11 … 6×11 7×9 8×9 9×9
        |   1:	0
        |   5:	0
        |  10:	0
        |  25:	2
        |  50:	4.5
        |  75:	7
        |  90:	8
        |  95:	9
        |  99:	9"""
    )
  }

  test("100 digits sample 4 only sample sorted") {
    check(
      shuffledDigits,
      numToSample = 4,
      onlySampleSorted = true,
      """N: 100, μ/σ: 4.3/2.9, med/mad: 4.5/2.5
        |sorted: 0×15 1×7 2×8 3×11 … 6×11 7×9 8×9 9×9
        |   1:	0
        |   5:	0
        |  10:	0
        |  25:	2
        |  50:	4.5
        |  75:	7
        |  90:	8
        |  95:	9
        |  99:	9"""
    )
  }

  val sortedShuffledDigits = shuffledDigits.sorted

  test("100 sorted digits") {
    check(
      sortedShuffledDigits,
      """N: 100, μ/σ: 4.3/2.9, med/mad: 4.5/2.5
        | elems: 0×15 1×7 2×8 3×11 4×9 5×12 6×11 7×9 8×9 9×9
        |   1:	0
        |   5:	0
        |  10:	0
        |  25:	2
        |  50:	4.5
        |  75:	7
        |  90:	8
        |  95:	9
        |  99:	9"""
    )
  }

  test("100 sorted digits only sample sorted overridden") {
    check(
      sortedShuffledDigits,
      numToSample = 4,
      onlySampleSorted = true,
      """N: 100, μ/σ: 4.3/2.9, med/mad: 4.5/2.5
        | elems: 0×15 1×7 2×8 3×11 … 6×11 7×9 8×9 9×9
        |   1:	0
        |   5:	0
        |  10:	0
        |  25:	2
        |  50:	4.5
        |  75:	7
        |  90:	8
        |  95:	9
        |  99:	9"""
    )
  }

  test("values over Int.MAX_VALUE") {
    check(
      Seq(
          10000000000L,
         100000000000L,
         100000000000L,
        1000000000000L,
        1000000000000L,
          10000000000L,
        1000000000000L,
         100000000000L,
          10000000000L,
          10000000000L
      ),
      """N: 10, μ/σ: 334000000000/437588848121.2, med/mad: 100000000000/90000000000
        | elems: 10000000000 100000000000×2 1000000000000×2 10000000000 1000000000000 100000000000 10000000000×2
        |sorted: 10000000000×4 100000000000×3 1000000000000×3
        |  10:	10000000000
        |  25:	10000000000
        |  50:	100000000000
        |  75:	1000000000000
        |  90:	1000000000000"""
    )
  }
}
