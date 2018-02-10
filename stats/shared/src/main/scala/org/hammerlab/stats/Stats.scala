package org.hammerlab.stats

import hammerlab.bool._
import hammerlab.iterator._
import hammerlab.print._
import hammerlab.show._
import org.hammerlab.io.Delimiter
import org.hammerlab.io.Delimiter.{ space, tab }
import org.hammerlab.math.interpolate
import spire.math.{ Integral, Numeric, Rational }
import spire.syntax.all._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.{ abs, ceil, floor, sqrt }

/**
 * Stores some computed statistics about a dataset of [[Numeric]] elements.
 *
 * Two concrete implementations are below: [[Empty]] and [[NonEmpty]].
 *
 * @tparam K [[Numeric]] element type. TODO(ryan): allow this to be non-[[Numeric]].
 * @tparam V [[Integral]] value type.
 */
sealed abstract class Stats[K: Numeric, V: Integral]

object Stats {

  /**
   * Construct a [[NonEmpty]] from a sequence of "runs"; elements paired with a count of repetitions.
   *
   * @param v values.
   * @param numToSample highlight this many "runs" of data from the start and end of the data; likewise the least and
   *                    greatest elements (and repetition counts).
   * @param onlySampleSorted only highlight the least and greatest elements; omit the first and last
   */
  def fromHist[K: Numeric: Ordering, V: Integral](v: Iterable[(K, V)],
                                                  numToSample: Int = 10,
                                                  onlySampleSorted: Boolean = false): Stats[K, V] = {

    var alreadySorted = true
    val hist = mutable.HashMap[K, V]()
    var n: V = 0

    val values = {
      val vBuilder = Vector.newBuilder[(K, V)]
      var prevOpt: Option[K] = None
      for {
        (value, num) ← v.runLengthReencode
      } {
        if (alreadySorted) {
          if (prevOpt.exists(_ > value))
            alreadySorted = false
          else
            prevOpt = Some(value)
        }
        vBuilder += value → num
        n += num
        hist.update(value, hist.getOrElse(value, 0: V) + num)
      }

      vBuilder.result()
    }

    if (values.isEmpty)
      return Empty[K, V]()

    val sorted =
      if (alreadySorted)
        values
      else
        for {
          key ← hist.keys.toVector.sorted
        } yield
          key → hist(key)

    val percentiles = histPercentiles(n, sorted)

    val median = percentiles(percentiles.length / 2)._2

    val medianDeviationsBuilder = Vector.newBuilder[(Double, V)]

    var sum = 0.0
    var sumSquares = 0.0
    for ((value, num) ← sorted) {
      val d = value.toDouble
      sum += d * num.toDouble()
      sumSquares += d * d * num.toDouble()
      medianDeviationsBuilder += abs(d - median) → num
    }

    val medianDeviations = medianDeviationsBuilder.result().sortBy(_._1)

    val mad =
      getRunPercentiles(
        medianDeviations,
        Seq(
          Rational(50) →
            (
              (n - 1) /~ 2,
              ((n - 1) % 2).toDouble() / 2.0
            )
        )
      )
      .head
      ._2

    val mean = sum / n.toDouble()
    val stddev = sqrt(sumSquares / n.toDouble() - mean * mean)

    def samples(vs: Vector[(K, V)]): Samples[K, V] =
      Samples[K, V](
        n,
        vs.take(numToSample),
        vs.takeRight(numToSample)
      )

    val samplesOpt =
      (alreadySorted || !onlySampleSorted) |
        samples(values)

    val sortedSamplesOpt =
      !alreadySorted |
        samples(sorted)

    NonEmpty(
      n,
      sum,
      mean, stddev,
      median, mad,
      samplesOpt,
      sortedSamplesOpt,
      percentiles
    )
  }

  /**
   * Construct a [[NonEmpty]] instance from input data `v`.
   *
   * @param v values.
   * @param numToSample highlight this many "runs" of data from the start and end of the data; likewise the least and
   *                    greatest elements (and repetition counts).
   * @param onlySampleSorted only highlight the least and greatest elements; omit the first and last
   */
  def apply[K: Numeric: Ordering](v: Iterable[K],
                                  numToSample: Int = 10,
                                  onlySampleSorted: Boolean = false): Stats[K, Int] = {

    val vBuilder = Vector.newBuilder[K]
    var alreadySorted = true
    var prevOpt: Option[K] = None
    for (value ← v) {
      if (alreadySorted) {
        if (prevOpt.exists(_ > value))
          alreadySorted = false
        else
          prevOpt = Some(value)
      }
      vBuilder += value
    }

    val values = vBuilder.result()

    if (values.isEmpty)
      return Empty[K, Int]()

    val n = values.length

    val sorted =
      if (alreadySorted)
        values
      else
        values.sorted

    val median = getMedian(sorted)

    val medianDeviationsBuilder = Vector.newBuilder[Double]

    var sum = 0.0
    var sumSquares = 0.0
    for (value ← sorted) {
      val d = value.toDouble
      sum += d
      sumSquares += d * d
      medianDeviationsBuilder += abs(d - median)
    }

    val medianDeviations = medianDeviationsBuilder.result().sorted
    val mad = getMedian(medianDeviations)

    val mean = sum / n
    val stddev = sqrt(sumSquares / n - mean * mean)

    def samples(vs: Vector[K]): Samples[K, Int] = {
      // Count occurrences of the first N distinct values.
      val (firstElems, numFirstElems) =
        runLengthEncodeWithSum(
          vs.iterator,
          numToSample
        )

      // Count occurrences of the last N distinct values.
      val (lastElems, numLastElems) =
        runLengthEncodeWithSum(
          vs.reverseIterator,
          numToSample,
          reverse = true
        )

      Samples(
        n,
        Runs(firstElems, numFirstElems),
        Runs(lastElems, numLastElems)
      )
    }

    val samplesOpt =
      (alreadySorted || !onlySampleSorted) |
        samples(values)

    val sortedSamplesOpt =
      !alreadySorted |
        samples(sorted)

    NonEmpty(
      n,
      sum,
      mean, stddev,
      median, mad,
      samplesOpt,
      sortedSamplesOpt,
      percentiles(sorted)
    )
  }

  /**
   * Compute percentiles listed in `ps` of the data in `values`; wrapper for implementation below.
   */
  private def getRunPercentiles[K: Numeric, V: Integral](values: Seq[(K, V)],
                                                         ps: Seq[(Rational, (V, Double))]): Percentiles[K] =
    getRunPercentiles(
      values
        .iterator
        .buffered,
      ps
        .iterator
        .buffered
    )
    .toVector

  /**
   * Compute percentiles listed in `ps` of the data in `values`.
   *
   * @param values runs of elements.
   * @param percentiles percentiles to compute, specified as tuples where the key is the percentile and the value is the
   *                    index in `values` at which that percentile lies (interpolated to be a fractional amount between
   *                    two indices, where appropriate).
   * @return pairs of (percentile, value).
   */
  private def getRunPercentiles[K: Numeric, V: Integral](values: BufferedIterator[(K, V)],
                                                         percentiles: BufferedIterator[(Rational, (V, Double))]): Iterator[(Rational, Double)] =
    new Iterator[(Rational, Double)] {

      var elemsPast: V = 0
      var curK: Option[K] = None

      override def hasNext: Boolean = percentiles.hasNext

      override def next(): (Rational, Double) = {
        val (percentile, (floor, remainder)) = percentiles.next()

        while(elemsPast <= floor) {
          val (k, v) = values.next()
          curK = Some(k)
          elemsPast += v
        }

        val distancePast = elemsPast - floor

        percentile →
          (
            if (distancePast == 1 && values.hasNext)
              interpolate(curK.get, values.head._1, remainder)
            else
              curK.get.toDouble()
          )
      }
    }

  private def percentileIdxs[K: Numeric, V: Integral](N: V): Vector[(Rational, (V, Double))] = {
    val n = N + 1

    implicit def fromInt = Integral[V].fromInt _

    val denominators: Iterator[V] = {
      lazy val pow10s: Stream[V] = 100 #:: pow10s.map(_ * 10)
      Iterator[V](
         2,  // 50
         4,  // 25/75
        10,  // 10/90
        20   //  5/95
      ) ++ pow10s.iterator  // 1/99, .1/99.9, .01/99.99, …
    }

    denominators
      .takeWhile(_ <= n)
      .flatMap {
        d ⇒
          val loPercentile = Rational(100, d.toSafeLong)
          val hiPercentile = 100 - loPercentile

          val loFloor: V = n /~ d - 1
          val loRemainder = (n % d).toDouble() / d.toDouble()

          val hiFloor = n - 3 - loFloor
          val hiRemainder = 1 - loRemainder

          if (d == 2)
            // Median (50th percentile, denominator 2) only emits one tuple.
            Iterator(loPercentile → (loFloor, loRemainder))
          else
            // In general, we emit two tuples per "denominator", one on the high side and one on the low. For example, for
          // denominator 4, we emit the 25th and 75th percentiles.
            Iterator(
              loPercentile → (loFloor, loRemainder),
              hiPercentile → (hiFloor, hiRemainder)
            )
      }
      .toVector
      .sortBy(_._1)
  }

  type Percentiles[K] = Vector[(Rational, PercentileValue[K])]

  /**
   * Compute some relevant percentiles based on the number of elements present.
   * @return pairs of (percentile, value).
   */
  private def histPercentiles[K: Numeric, V: Integral](N: V,
                                                       values: IndexedSeq[(K, V)]): Percentiles[K] =
    getRunPercentiles(
      values,
      percentileIdxs(N)
    )

  /**
   * Compute some relevant percentiles based on the number of elements present.
   *
   * @return pairs of (percentile, value).
   */
  private def percentiles[T: Numeric](values: IndexedSeq[T]): Vector[(Rational, Double)] =
    percentileIdxs(values.length)
      .map {
        case (d, (floor, weight)) ⇒
          val value =
            if (weight == 0)
              values(floor).toDouble()
            else
              interpolate(values(floor), values(floor + 1), weight)

          d → value
      }

  private def getMedian[T: Numeric](sorted: Vector[T]): Double = {
    val n = sorted.length
    if (n % 2 == 0)
      (sorted(n / 2 - 1) + sorted(n / 2)).toDouble() / 2.0
    else
      sorted(n / 2).toDouble()
  }

  /**
   * Find the first `N` "runs" from the beginning of `it`. If `reverse`, return them in reversed order.
   */
  private def runLengthEncodeWithSum[K: Numeric: Ordering](it: Iterator[K],
                                                           N: Int,
                                                           reverse: Boolean = false): (Seq[(K, Int)], Int) = {
    var sum = 0
    var i = 0
    val runs = ArrayBuffer[(K, Int)]()
    val runLengthIterator = it.runLengthEncode
    while (i < N && runLengthIterator.hasNext) {
      val (elem, count) = runLengthIterator.next()

      if (reverse)
        runs.prepend(elem → count)
      else
        runs += ((elem, count))

      sum += count
      i += 1
    }
    runs → sum
  }

  /**
   * Default [[Show]] implementation, utilizing [[Show]]s for key- and value-types as well as [[Rational]] percentiles
   * and a [[Double]] percentile values and summary statistics.
   *
   * Example with shuffled positive digits (from tests):
   *
   * {{{
   * N: 9, μ/σ: 5/2.6, med/mad: 5/2
   *  elems: 8 4 5 … 7 2 9
   * sorted: 1 2 3 … 7 8 9
   *   10:	1
   *   25:	2.5
   *   50:	5
   *   75:	7.5
   * }}}
   */
  implicit def makeShow[
    K : Numeric : Show,
    V: Integral : Show
  ](
    implicit
    percentileShow: Show[Rational] = showPercentile,
    statShow: Show[Double] = showDouble,
    delimiter: Delimiter = space,
    indent: Indent = hammerlab.indent.tab
  ): ToLines[Stats[K, V]] =
    ToLines {
      case Empty() ⇒ "(empty)": Lines
      case NonEmpty(n, _, mean, stddev, median, mad, samplesOpt, sortedSamplesOpt, percentiles) ⇒
        def pair[L: Show, R: Show](l: L,
                                   r: R,
                                   d: Delimiter = delimiter): String =
          show"$l:$d$r"

        Lines(
          (
            List(
              pair("N", n),
              pair("μ/σ", show"$mean/$stddev")
            ) ++
            (
              if (n > 2)
                List(pair("med/mad", show"$median/$mad"))
              else
                Nil
              )
          )
          .mkString(", "),

          for {
            samples ← samplesOpt
            if samples.nonEmpty
          } yield
            pair(" elems", samples),

          for {
            sortedSamples ← sortedSamplesOpt
            if sortedSamples.nonEmpty
          } yield
            pair("sorted", sortedSamples),

          /**
           * Show percentiles iff one of the [[Samples]] fields has elided elements
           */
          if (n >= 5)
            percentiles.map {
              case (k, v) ⇒
                pair(k, v, tab)
            }
          else
            Lines()
        )
    }

  /**
   * Default [[Show]] for summary statistics and percentile values
   */
  def showDouble: Show[Double] =
    Show(
      d ⇒
        if (floor(d).toLong == ceil(d).toLong)
          d.toLong.toString
        else
          "%.1f".format(d)
    )

  /**
   * Default [[Show]] for percentile-keys (generally integers except for on the edges in large datasets, where
   * percentiles of rthe forms 1e-N and 1-1e-N are included).
   */
  def showPercentile(implicit
                     showDouble: Show[Double] = cats.implicits.catsStdShowForDouble,
                     showLong: Show[Long] = showLong): Show[Rational] =
    Show(
      r ⇒
        "%4s".format(
          if (r.isWhole())
            r.toLong.show
          else
            r.toDouble.show
        )
    )
}

case class Empty[K: Numeric, V: Integral]()
  extends Stats[K, V]

/**
 * Stores some computed statistics about a dataset of [[Numeric]] elements.
 *
 * @param n number of elements in the dataset.
 * @param mad median absolute deviation (from the median).
 * @param samplesOpt "sample" elements; the start and end of the data.
 * @param sortedSamplesOpt "sample" elements; the least and greatest elements. If the dataset is already sorted, meaning
 *                         this would be equivalent to [[samplesOpt]], it is omitted.
 * @param percentiles selected percentiles of the dataset.
 */
case class NonEmpty[K: Numeric, V: Integral](n: V,
                                             sum: Double,
                                             mean: Double,
                                             stddev: Double,
                                             median: Double,
                                             mad: Double,
                                             samplesOpt: Option[Samples[K, V]],
                                             sortedSamplesOpt: Option[Samples[K, V]],
                                             percentiles: Seq[(Rational, Double)])
  extends Stats[K, V]
