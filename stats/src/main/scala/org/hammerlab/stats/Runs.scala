package org.hammerlab.stats

import cats.Show
import cats.Show.show
import cats.implicits._
import org.hammerlab.io.Delimiter
import spire.implicits._
import spire.math.Integral

/**
 * Convenience class wrapping a sequence of key-number pairs, used in run-length-encoding in [[NonEmpty]].
 */
case class Runs[K, V: Integral](elems: Seq[(K, V)], num: V)

object Runs {
  implicit def runsToSeq[K, V: Integral](runs: Runs[K, V]): Seq[(K, V)] = runs.elems
  implicit def seqToRuns[K, V: Integral](elems: Seq[(K, V)]): Runs[K, V] = apply[K, V](elems)

  def apply[K, V: Integral](elems: Seq[(K, V)]): Runs[K, V] =
    Runs(
      elems,
      elems
        .map(_._2)
        .reduce(_ + _)
    )

  implicit def makeShow[K: Show, V : Integral : Show](implicit delimiter: Delimiter): Show[Runs[K, V]] =
    show {
      case Runs(elems, _) ⇒
        elems
          .map {
            case (elem, count) ⇒
              if (count == 1)
                elem.show
              else
                show"$elem×$count"
          }
          .mkString(delimiter)
    }
}
