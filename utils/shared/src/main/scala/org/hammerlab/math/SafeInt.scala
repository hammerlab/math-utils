package org.hammerlab.math

import cats.implicits._

object SafeInt {
  implicit class Ops(val n: Long) extends AnyVal {
    def safeInt: Either[CastException, Int] =
      if (n > Integer.MAX_VALUE)
        Left(
          CastException(n)
        )
      else
        Right(
          n.toInt
        )

    def safeInt(msg: String): Either[IllegalArgumentException, Int] = safeInt(new IllegalArgumentException(msg, _))
    def safeInt[E <: Exception](wrap: CastException ⇒ E): Either[E, Int] = safeInt.leftMap(wrap)
  }

  trait syntax {
    @inline implicit def makeSafeIntOps(n: Long): Ops = Ops(n)
  }
}

case class CastException(value: Long)
  extends RuntimeException(
    s"Attempting to cast $value to an integer"
  )
