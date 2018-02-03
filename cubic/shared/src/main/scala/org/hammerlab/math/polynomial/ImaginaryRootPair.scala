package org.hammerlab.math.polynomial

import org.hammerlab.math.syntax.{ Arithmetic, Doubleish }
import spire.algebra.Ring
import spire.math.Complex

sealed trait Result[T] {
//  def +(d: Double): Result[T]
//  def -(d: Double): Result[T]
//  def *(d: Double): Result[T]
//  def /(d: Double): Result[T]
}

//object Result {
//  def doubleArithmetic[T] =
//    new Arithmetic.D[Result[T]] {
//      override def +(l: Result[T], r: Double): Result[T] = l + r
//      override def -(l: Result[T], r: Double): Result[T] = l - r
//      override def *(l: Result[T], r: Double): Result[T] = l * r
//      override def /(l: Result[T], r: Double): Result[T] = l / r
//    }
//}

import Arithmetic._
case class Real[T](t: T) extends Result[T] {

//  override def +(d: Double) = Real(t + d)
//  override def -(d: Double) = Real(t - d)
//  override def *(d: Double) = Real(t * d)
//  override def /(d: Double) = Real(t / d)
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

//  override def +(d: Double): ImaginaryRootPair[T] = ImaginaryRootPair(a + d, b)
//  override def -(d: Double): ImaginaryRootPair[T] = ImaginaryRootPair(a - d, b)
//  override def *(d: Double): ImaginaryRootPair[T] = ImaginaryRootPair(a * d, b)
//  override def /(d: Double): ImaginaryRootPair[T] = ImaginaryRootPair(a / d, b)
}

object ImaginaryRootPair {
  import spire.implicits._
  implicit class ImaginaryRootPairOps[T](r: ImaginaryRootPair[T]) {
    def complex(implicit e: Ring[T]): Seq[Complex[T]] =
      Seq(
        Complex(r.a, -r.b),
        Complex(r.a, r.b)
      )
  }
}
