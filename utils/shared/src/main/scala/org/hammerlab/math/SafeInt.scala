package org.hammerlab.math

import cats.implicits._

import scala.util.Try

object SafeInt {
  implicit class Ops(val n: Long) extends AnyVal {
    @inline def safeInt: Either[CastException, Int] = safeInt()
    def safeInt(msg: String = ""): Either[CastException, Int] =
      if (n > Integer.MAX_VALUE || n < Integer.MIN_VALUE)
        Left(
          CastException(n, msg)
        )
      else
        Right(
          n.toInt
        )

    @inline def int_! : Int = int_!()
    def int_!(msg: String = ""): Int = safeInt(msg).fold(throw _, x ⇒ x)
  }

  trait syntax {
    @inline implicit def makeSafeIntOps(n: Long): Ops = Ops(n)
    type CastException = org.hammerlab.math.CastException
  }
}

case class CastException(value: Long, msg: String = "")
  extends RuntimeException(
    if (msg.nonEmpty)
      s"$msg: attempting to cast $value to an integer"
    else
      s"Attempting to cast $value to an integer"
  )
