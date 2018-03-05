package org.hammerlab.math.syntax

/**
 * Type-class for types that can be converted to a [[Double]], e.g. for [[FuzzyCmp]] purposes
 */
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
    def toInt   (implicit d: Doubleish[T]): Int    = d(t).toInt
  }

  implicit val bigDecimal: Doubleish[BigDecimal] =
    new Doubleish[BigDecimal] {
      override def apply(t: BigDecimal): Double = t.doubleValue()
    }
}
