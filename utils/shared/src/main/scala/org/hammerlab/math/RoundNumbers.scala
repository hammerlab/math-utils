package org.hammerlab.math

import spire.math.Integral
import spire.implicits._

/**
 * Emit an exponentially-increasing sequence of integers composed of repetitions of `steps` scaled by successive powers
 * of `base`.
 */
class RoundNumbers[I: Integral] private(steps: Seq[Int],
                                        base: Int = 10,
                                        limitOpt: Option[I])
  extends Iterator[I] {

  var n: Option[I] = None
  private var idx = 0
  private var basePow: I = Integral[I].one

  override def hasNext: Boolean = {
    if (n.isEmpty) {
      val next = steps(idx) * basePow
      if (limitOpt.exists(_ < next)) {
        return false
      } else
        n = Some(next)
    }
    true
  }

  override def next(): I = {
    if (!hasNext) throw new NoSuchElementException
    val r = n.get
    n = None
    idx += 1
    if (idx == steps.size) {
      idx = 0
      basePow *= base
    }
    r
  }
}

/**
 * Constructors.
 */
object RoundNumbers {
  def apply[I: Integral](steps: Seq[Int],
                         limit: I,
                         base: Int = 10): Iterator[I] =
    new RoundNumbers(steps, base, Some(limit))

  def apply(steps: Seq[Int],
            base: Int): Iterator[Long] =
    new RoundNumbers[Long](steps, base, None)
}
