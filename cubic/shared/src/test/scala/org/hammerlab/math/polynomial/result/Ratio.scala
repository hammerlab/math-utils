package org.hammerlab.math.polynomial.result

import hammerlab.show._
import org.hammerlab.math.syntax.Doubleish
import org.hammerlab.math.syntax.Doubleish._
import spire.algebra.{ Field, IsReal, NRoot, Order, Trig }
import spire.implicits._
import spire.math.{ Complex, log, max }

sealed trait Ratio

case object       LeftZero extends Ratio
case object      RightZero extends Ratio
case  class Log(v: Double) extends Ratio

object Ratio {
  def apply[D: Doubleish : Field : Trig : Order : IsReal : NRoot](l: Complex[D], r: Complex[D]): Ratio =
    (
      l.abs,
      r.abs,
      (l - r).abs
    ) match {
      case (0, 0, _) ⇒ Log(0)
      case (0, _, _) ⇒ LeftZero
      case (_, 0, _) ⇒ RightZero
      case (l, r, d) ⇒
        Log(
          max(
            log(l + d) - log(l),
            log(r + d) - log(r)
          )
          .toDouble
        )
    }

  implicit val ord: Ordering[Ratio] =
    new Ordering[Ratio] {
      override def compare(x: Ratio, y: Ratio): Int =
        (x, y) match {
          case (            LeftZero,            RightZero) ⇒  1
          case (           RightZero,             LeftZero) ⇒ -1
          case (LeftZero | RightZero,               Log(_)) ⇒  1
          case (              Log(_), LeftZero | RightZero) ⇒ -1
          case (              Log(l),               Log(r)) ⇒  Ordering[Double].compare(l, r)
          case _ ⇒ 0
        }
    }
}
