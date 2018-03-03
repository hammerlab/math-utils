package org.hammerlab.math.syntax

import hammerlab.math.tolerance._
import spire.math.Complex
import spire.implicits._

/**
 * Type-class with various math-operations
 */
trait Math[D] extends Any {

  def ^(base: D, exp: Double): D

  def unary_-(t: D): D

  def sqrt(t: D)(implicit ε: E): D
  def cbrt(t: D): D = ^(t, 1.0/3)

  def cos(t: D): D
  def acos(t: D)(implicit ε: E): D
}

object Math {

  def apply[T](implicit n: Math[T]): Math[T] = n

  implicit class MathOps[D](d: D)(implicit n: Math[D]) {
    def ^(p: Double): D = n.^(d, p)

    def unary_- : D = n.unary_-(d)

    def sqrt(implicit ε: E): D = n.sqrt(d)
    def cbrt: D = n.cbrt(d)

    def cos: D = n.cos(d)
    def acos(implicit ε: E): D = n.acos(d)
  }

  implicit val double =
    new Math[Double] {
      override def ^(base: Double, exp: Double): Double = math.pow(base, exp)
      override def unary_-(t: Double): Double = -t
      override def sqrt(t: Double)(implicit ε: E): Double =
        if (t < 0)
          if (t >= -ε)
            0
          else
            throw new IllegalArgumentException(s"Illegal sqrt: $t")
        else
          math.sqrt(t)
      override def cbrt(t: Double): Double = math.cbrt(t)
      override def cos(t: Double): Double = math.cos(t)
      override def acos(t: Double)(implicit ε: E): Double =
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

  implicit val complex =
    new Math[Complex[Double]] {
      override def ^(base: Complex[Double], exp: Double): Complex[Double] = base.pow(exp)
      override def unary_-(t: Complex[Double]): Complex[Double] = -t
      override def sqrt(t: Complex[Double])(implicit ε: E): Complex[Double] = t.sqrt
      override def cos(t: Complex[Double]): Complex[Double] = t.cos
      override def acos(t: Complex[Double])(implicit ε: E): Complex[Double] = t.acos
    }
}
