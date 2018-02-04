package org.hammerlab.math.polynomial

import org.hammerlab.math.syntax.{ Doubleish, FuzzyCmp, Tolerance }
import spire.algebra.{ Field, Ring, Signed }
import spire.implicits._
import spire.math.{ Complex, abs }

sealed trait Result[T]

case class Real[T](t: T) extends Result[T] {
  override def toString: String = s"$t"
}

object Real {
  def doubleish[T](implicit d: Doubleish[T]): Doubleish[Real[T]] =
    new Doubleish[Real[T]] {
      override def apply(t: Real[T]): Double = d(t.t)
    }
}

/**
 * A pair of conjugate imaginary numbers, [[a]]±[[b]]i, both of which are roots of a quartic polynomial
 *
 * We're testing polynomials with real coefficients, so imaginary roots must come in conjugate-pairs
 *
 * [[b]] must be greater than zero; real roots are not modeled with this data structure
 */
case class ImaginaryRootPair[T](a: T, b: T) extends Result[T] {
  override def toString: String =
    (if (a == 0) "" else s"$a") +
      "±" +
      (if (b == 1) "" else s"$b") +
      "i"
}

object ImaginaryRootPair {
  implicit class ImaginaryRootPairOps[T](r: ImaginaryRootPair[T]) {
    def complex(implicit e: Ring[T]): Seq[Complex[T]] =
      Seq(
        Complex(r.a, -r.b),
        Complex(r.a, r.b)
      )
  }

  def pairs[D: Field : Signed](imags: List[Complex[D]])(
      implicit
        ε: Tolerance,
      ord: Ordering[D],
      cmp: FuzzyCmp[D, D]
  )
         : List[ImaginaryRootPair[D]] =
    imags match {
      case Nil ⇒ Nil
      case head :: tail ⇒
        val conj = head.conjugate
        val next = List.newBuilder[Complex[D]]
        var min: Option[(Complex[D], D)] = None
        tail
          .foreach {
            imag ⇒
              val Complex(a, b) = imag - conj
              val distance = a*a + b*b
              min match {
                case None ⇒
                  min = Some(imag → distance)
                case Some((prev, m)) if ord.lt(distance, m) ⇒
                  next += prev
                  min = Some(imag → distance)
                case _ ⇒
                  next += imag
              }
          }

        import org.hammerlab.math.syntax.FuzzyCmp._
        (
          (head, min) match {
            case (Complex(a1, b1), Some((Complex(a2, b2), d))) ⇒
              val m = Seq(a1, b1, a2, b2).max
              if (m + d !== m)
                throw new IllegalArgumentException(
                  s"Bad conjugate pairs: $head $min, distance: $d imags: $imags"
                )
              ImaginaryRootPair(
                    (a1 + a2) / 2,
                abs((b1 - b2) / 2)
              )
            case _ ⇒
              throw new IllegalArgumentException(s"Odd number of roots to pair? $imags")
          }
        ) :: pairs(next.result())

    }
}
