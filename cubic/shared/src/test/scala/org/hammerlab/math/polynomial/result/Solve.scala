package org.hammerlab.math.polynomial.result

import org.hammerlab.math.polynomial.TestCase
import spire.math.Complex

object Solve {
  def apply[D](implicit s: Solve[D]): Solve[D] = s
}

case class Solve[D](f: TestCase[D] ⇒ Seq[Complex[D]]) {
  def apply(t: TestCase[D]): Seq[Complex[D]] = f(t)
}
