package org.hammerlab.math.syntax

import spire.math.Complex
import spire.implicits._

trait Doubleish[-T] {
  def apply(t: T): Double
}

object Doubleish {
  implicit val id: Doubleish[Double] =
    new Doubleish[Double] {
      override def apply(t: Double): Double = t
    }

  implicit val int: Doubleish[Int] =
    new Doubleish[Int] {
      override def apply(t: Int): Double = t
    }

  implicit val long: Doubleish[Long] =
    new Doubleish[Long] {
      override def apply(t: Long): Double = t
    }

  implicit def ord[D](implicit d: Doubleish[D]): Ordering[D] =
    Ordering.by[D, Double](d(_))(Ordering.Double)

  implicit class DoubleishOps[T](val t: T) extends AnyVal {
    def toDouble(implicit d: Doubleish[T]): Double = d(t)
  }

  implicit val bigDecimal: Doubleish[BigDecimal] =
    new Doubleish[BigDecimal] {
      override def apply(t: BigDecimal): Double = t.doubleValue()
    }

//  implicit def complex: Doubleish[Complex[Double]] =
//    new Doubleish[Complex[Double]] {
//      override def apply(c: Complex[Double]): Double = c.norm
//    }
}
