package org.hammerlab.math.syntax

/**
 * Type-class with various math-operations
 */
trait Math[D] extends Any {

  def ^(base: D, exp: Double): D

  def unary_-(t: D): D

  def sqrt(t: D)(implicit ε: Tolerance): D
  def cbrt(t: D): D = ^(t, 1.0/3)

  def cos(t: D): D
  def acos(t: D)(implicit ε: Tolerance): D
}

object Math {

  def apply[T](implicit n: Math[T]): Math[T] = n

  implicit class MathOps[D](d: D)(implicit n: Math[D]) {
    def ^(p: Double): D = n.^(d, p)

    def unary_- : D = n.unary_-(d)

    def sqrt(implicit ε: Tolerance): D = n.sqrt(d)
    def cbrt: D = n.cbrt(d)

    def cos: D = n.cos(d)
    def acos(implicit ε: Tolerance): D = n.acos(d)
  }

  implicit val double =
    new Math[Double] {
      override def ^(base: Double, exp: Double): Double = math.pow(base, exp)
      override def unary_-(t: Double): Double = -t
      override def sqrt(t: Double)(implicit ε: Tolerance): Double =
        if (t < 0)
          if (t >= -ε)
            0
          else
            throw new IllegalArgumentException(s"Illegal sqrt: $t")
        else
          math.sqrt(t)
      override def cbrt(t: Double): Double = math.cbrt(t)
      override def cos(t: Double): Double = math.cos(t)
      override def acos(t: Double)(implicit ε: Tolerance): Double =
        if (t > 1)
          if (t <= 1 + ε)
            0
          else
            throw new IllegalArgumentException(s"Illegal arccos: $t")
        else if (t < -1)
          if (t >= -1 - ε)
            math.Pi
          else
            throw new IllegalArgumentException(s"Illegal arccos: $t")
        else
          math.acos(t)
    }
}
