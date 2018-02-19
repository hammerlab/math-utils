package cubic.complex

import java.math.MathContext.DECIMAL128

trait FromDouble[T] {
  def apply(d: Double): T
}

object FromDouble {
  def apply[T](d: Double)(implicit f: FromDouble[T]): T = f(d)
  def instance[T](fn: Double ⇒ T): FromDouble[T] =
    new FromDouble[T] {
      override def apply(d: Double): T = fn(d)
    }

  implicit val double: FromDouble[Double] = instance(x ⇒ x)
  implicit val bigDecimal: FromDouble[BigDecimal] =
    instance(
      BigDecimal(
        _,
        DECIMAL128
      )
    )
}
