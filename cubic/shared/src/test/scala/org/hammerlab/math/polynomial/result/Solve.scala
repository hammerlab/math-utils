package org.hammerlab.math.polynomial.result

import org.hammerlab.math.polynomial.TestCase
import spire.math.Complex

object Solve {
  def apply[D](implicit s: Solve[D]): Solve[D] = s
  def makeSolve[D](implicit fn: TestCase[D] ⇒ Seq[Complex[D]]): Solve[D] =
    new Solve[D] {
      override def apply(t: TestCase[D]): Seq[Complex[D]] = fn(t)
    }
}

trait Solve[D] extends (TestCase[D] ⇒ Seq[Complex[D]])
