package org.hammerlab.math.syntax

class Tolerance private(val ε: Double) extends AnyVal {
  override def toString: String = s"±$ε"
}
object Tolerance {
  implicit def wrap(v: Double): Tolerance = new Tolerance(1 + v)
  implicit def unwrap(t: Tolerance): Double = t.ε
}
