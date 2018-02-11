package org.hammerlab.stats

//import org.hammerlab.math.syntax.Arithmetic
import spire.algebra.Signed
import spire.math.Rational
//import spire.math.abs
import spire.syntax.all._

case class Interpolated[T](lo: T, hi: T, weight: Rational) {
  def -(r: T)(implicit n: spire.math.Numeric[T]): Interpolated[T] =
    copy(
      lo = lo - r,
      hi = hi - r
    )

  def abs(implicit s: Signed[T]): Interpolated[T] =
    copy(
      lo = spire.math.abs(lo),
      hi = spire.math.abs(hi)
    )
}

object Interpolated {
  def apply[T](t: T): Interpolated[T] = Interpolated(t, t, 0)

/*
  import Arithmetic._
  implicit def arithmetic[T: Arithmetic.I]: Arithmetic[Interpolated[T], T] =
    new Arithmetic[Interpolated[T], T] {
      override def +(l: Interpolated[T], r: T): Interpolated[T] = Interpolated(l.lo + r, l.hi + r, l.weight)
      override def -(l: Interpolated[T], r: T): Interpolated[T] = Interpolated(l.lo - r, l.hi - r, l.weight)
      override def *(l: Interpolated[T], r: T): Interpolated[T] = Interpolated(l.lo * r, l.hi * r, l.weight)
      override def /(l: Interpolated[T], r: T): Interpolated[T] = Interpolated(l.lo / r, l.hi / r, l.weight)
    }
*/
}

