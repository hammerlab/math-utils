package org.hammerlab.math.syntax

class Tolerance private(val ε: Double) extends AnyVal {
  override def toString: String = s"±$ε"
}
object Tolerance {
  implicit def wrap(v: Double): Tolerance = new Tolerance(1 + v)
  implicit def unwrap(t: Tolerance): Double = t.ε
}

/*
import cats.Eq
import org.hammerlab.math.format.SigFigs

import scala.math.pow

sealed trait Tolerance
object Tolerance {
  implicit def doubleEq(implicit ε: Tolerance): Eq[Double] =
    ε match {
      case a: Absolute ⇒ Absolute.doubleEq(a)
      case r: Relative ⇒ Relative.doubleEq(r)
    }
}

case class Absolute(v: Double) extends Tolerance

object Absolute {
  implicit def wrap(v: Double): Absolute = Absolute(v)
  implicit def unwrap(t: Absolute): Double = t.v

  implicit def doubleEq(implicit ε: Absolute) = new Eq[Double] {
    override def eqv(a: Double, b: Double): Boolean =
      a + ε >= b &&
        b + ε >= a
  }
}

case class Relative(ε: Double) extends Tolerance

object Relative {
  implicit def wrap(v: Double): Relative = Relative(1 + v)
  implicit def unwrap(t: Relative): Double = t.ε

  implicit def fromSigFigs(implicit s: SigFigs): Relative = Relative(pow(10, -s.n))

  implicit def doubleEq(implicit ε: Relative): Eq[Double] =
    new Eq[Double] {
      override def eqv(a: Double, b: Double): Boolean =
        if (a < 0)
          a * ε <= b &&
            b * ε <= a
        else
          a * ε >= b &&
            b * ε >= a
    }
}
*/
