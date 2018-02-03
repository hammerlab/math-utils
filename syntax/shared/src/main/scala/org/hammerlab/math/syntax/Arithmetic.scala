package org.hammerlab.math.syntax

import Arithmetic.ReverseArithmeticOps
import spire.math.Complex

trait Arithmetic[L, R] {
  def +(l: L, r: R): L
  def -(l: L, r: R): L
  def *(l: L, r: R): L
  def /(l: L, r: R): L
}

trait LowPriArithmetic {
  implicit def makeReverseOps[R](r: R): ReverseArithmeticOps[R] = new ReverseArithmeticOps(r)
}

object Arithmetic
  extends LowPriArithmetic {

  type I[T] = Arithmetic[T, T]
  type D[T] = Arithmetic[T, Double]

  implicit class ArithmeticOps[L](val l: L) extends AnyVal {
    def +[R](r: R)(implicit a: Arithmetic[L, R]): L = a.+(l, r)
    def -[R](r: R)(implicit a: Arithmetic[L, R]): L = a.-(l, r)
    def *[R](r: R)(implicit a: Arithmetic[L, R]): L = a.*(l, r)
    def /[R](r: R)(implicit a: Arithmetic[L, R]): L = a./(l, r)
  }

  class ReverseArithmeticOps[R](val r: R) extends AnyVal {
    def +[L](l: L)(implicit a: Arithmetic[L, R]): L = a.+(l, r)
    def -[L](l: L)(implicit a: Arithmetic[L, R]): L = a.-(l, r)
    def *[L](l: L)(implicit a: Arithmetic[L, R]): L = a.*(l, r)
    def /[L](l: L)(implicit a: Arithmetic[L, R]): L = a./(l, r)
  }

  implicit def intFromDouble[T](implicit a: Arithmetic[T, Double]): Arithmetic[T, Int] =
    new Arithmetic[T, Int] {
      override def -(l: T, r: Int): T = l - r.toDouble
      override def +(l: T, r: Int): T = l + r.toDouble
      override def *(l: T, r: Int): T = l * r.toDouble
      override def /(l: T, r: Int): T = l / r.toDouble
    }

  implicit val double: Arithmetic.I[Double] =
    new Arithmetic.I[Double] {
      override def +(l: Double, r: Double): Double = l + r
      override def -(l: Double, r: Double): Double = l - r
      override def *(l: Double, r: Double): Double = l * r
      override def /(l: Double, r: Double): Double = l / r
    }

  import spire.implicits._

  implicit val complex =
    new Arithmetic.I[Complex[Double]] {
      override def +(l: Complex[Double], r: Complex[Double]): Complex[Double] = l + r
      override def -(l: Complex[Double], r: Complex[Double]): Complex[Double] = l - r
      override def *(l: Complex[Double], r: Complex[Double]): Complex[Double] = l * r
      override def /(l: Complex[Double], r: Complex[Double]): Complex[Double] = l / r
    }

  implicit val complexDouble =
    new Arithmetic.D[Complex[Double]] {
      override def +(l: Complex[Double], r: Double): Complex[Double] = l + r
      override def -(l: Complex[Double], r: Double): Complex[Double] = l - r
      override def *(l: Complex[Double], r: Double): Complex[Double] = l * r
      override def /(l: Complex[Double], r: Double): Complex[Double] = l / r
    }
}
