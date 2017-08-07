package org.hammerlab.math

import cats.Show
import cats.Show.show
import spire.implicits._
import spire.math.Integral

import scala.math.round

object Format {
  def scientific[I: Integral](n: I, precision: Int): String =
    if (n < 0)
      s"-${scientific(-n, precision)}"
    else {
      assert(precision >= 2)
      val digits = n.toString
      val numDigits = digits.length
      if (numDigits > precision + 3) {

        val integral = implicitly[Integral[I]]
        import integral.fromDouble

        val roundedDigits =
          fromDouble(
            round(
              s"${digits.substring(0, precision)}.${digits(precision)}"
                .toDouble
            )
          )
          .toString

        val first = roundedDigits.head
        val rest = roundedDigits.substring(1, precision)

        s"$first.${rest}e${numDigits - 1 + roundedDigits.length - precision}"
      } else
        digits
    }

  def scientific[I: Integral](precision: Int): Show[I] =
    show(scientific(_, precision))

  object scientific {
    implicit def digits2[I: Integral] = scientific[I](2)
    implicit def digits3[I: Integral] = scientific[I](3)
    implicit def digits4[I: Integral] = scientific[I](3)
  }
}
