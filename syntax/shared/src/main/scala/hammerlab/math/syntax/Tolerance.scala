package hammerlab.math.syntax

case class Tolerance(ε: Double)
object Tolerance {
  implicit def wrap(v: Double): Tolerance = Tolerance(v)
  implicit def unwrap(t: Tolerance): Double = t.ε
}
