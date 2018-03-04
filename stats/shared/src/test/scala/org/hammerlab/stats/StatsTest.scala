package org.hammerlab.stats

import org.hammerlab.Suite
import spire.implicits._
import spire.math.Numeric

import scala.math.sqrt

class StatsTest
  extends Suite {

  def check[K : Numeric : Ordering](input: Seq[K],
                                    expected: Stats[K, Int]): Unit =
    Stats(input) should be(expected)

  def check[K : Numeric : Ordering](input: Seq[K],
                                    numToSample: Int,
                                    expected: Stats[K, Int]): Unit =
    Stats(
      input,
      numToSample
    ) should be(
      expected
    )

  def check[K : Numeric : Ordering](input: Seq[K],
                                    numToSample: Int,
                                    onlySampleSorted: Boolean,
                                    expected: Stats[K, Int]): Unit =
    Stats(
      input,
      numToSample,
      onlySampleSorted
    ) should be(
      expected
    )

  test("empty") {
    check[Int](
      Nil,
      Empty[Int, Int]()
    )
  }

  test("0 to 0") {
    check(
      0 to 0,
      NonEmpty(
        n = 1,
        sum = 0,
        mean = 0,
        stddev = 0,
        median = 0,
        mad = 0,
        samplesOpt =
          Some(
            Samples(
              1,
              Runs(Seq(0 → 1)),
              Runs(Seq(0 → 1))
            )
          ),
        sortedSamplesOpt = None,
        percentiles = Vector(Mid(50) → 0.0)
      )
    )
  }

  test("0 to 1") {
    check(
      0 to 1,
      NonEmpty(
        n = 2,
        sum = 1,
        mean = .5,
        stddev = .5,
        median = .5,
        mad = .5,
        samplesOpt =
          Some(
            Samples(
              2,
              Runs(Seq(0 → 1, 1 → 1)),
              Runs(Seq(0 → 1, 1 → 1))
            )
          ),
        sortedSamplesOpt = None,
        percentiles = Vector(Mid(50) → .5)
      )
    )
  }

  test("1 to 0") {
    check(
      1 to 0 by -1,
      NonEmpty(
        n = 2,
        sum = 1,
        mean = .5,
        stddev = .5,
        median = .5,
        mad = .5,
        samplesOpt =
          Some(
            Samples(
              2,
              Runs(Seq(1 → 1, 0 → 1)),
              Runs(Seq(1 → 1, 0 → 1))
            )
          ),
        sortedSamplesOpt =
          Some(
            Samples(
              2,
              Runs(Seq(0 → 1, 1 → 1)),
              Runs(Seq(0 → 1, 1 → 1))
            )
          ),
        percentiles = Vector(Mid(50) → .5)
      )
    )
  }

  test("0 to 2") {
    check(
      0 to 2,
      NonEmpty(
        n = 3,
        sum = 3,
        mean = 1,
        stddev = sqrt(2 / 3.0),
        median = 1,
        mad = 1,
        samplesOpt =
          Some(
            Samples(
              3,
              Runs(Seq(0 → 1, 1 → 1, 2 → 1)),
              Runs(Seq(0 → 1, 1 → 1, 2 → 1))
            )
          ),
        sortedSamplesOpt = None,
        percentiles = Vector[(Percentile, Double)](Mid(25) → 0, Mid(50) → 1, Mid(75) → 2)
      )
    )
  }

}
