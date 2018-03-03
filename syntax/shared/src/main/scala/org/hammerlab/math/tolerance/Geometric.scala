package org.hammerlab.math.tolerance

import cats.Show
import cats.syntax.show._

/**
 * Consider [[Double]]s to be "equal" if they are within a factor of [[ε]] of one another:
 *
 * {{{
 * import hammerlab.math.syntax._
 * implicit val ε: E = 1e-6
 * 2.0 === 2.000002      //  true
 * 2.0 !== 2.0000020001  // false
 * 2.0 <<< 2.0000020001  //  true
 * 2.0 <<= 2.0000020001  //  true
 * 2.0  ≤  2.0000020001  //  true
 * }}}
 *
 * Note that equality (and by extension, ≤ and ≥) are not transitive! Don't try to use this as an [[Ordering]]!
 *
 * {{{
 * implicit val ε: E = 1e-2
 * 2.0  === 2.02  //  true
 * 2.02 === 2.04  //  true
 * 2.0  === 2.04  // false!
 * }}}
 */
class Geometric private(val ε: Double) extends AnyVal {
  override def toString: String = s"±$ε"

  import Geometric.D

  def <(l: D, r: D): Boolean =
    if (l < 0 && r < 0)
      l < r * ε
    else
      l * ε < r

  @inline def ===(l: D, r: D): Boolean = ! >(l, r) && ! <(l, r)
  @inline def !==(l: D, r: D): Boolean =   >(l, r) ||   <(l, r)
  @inline def  <>(l: D, r: D): Boolean =   >(l, r) ||   <(l, r)

  @inline def   >(l: D, r: D): Boolean =   <(r, l)
  @inline def  >=(l: D, r: D): Boolean = ! <(l, r)
  @inline def   ≥(l: D, r: D): Boolean = ! <(l, r)

  @inline def  <=(l: D, r: D): Boolean = ! >(l, r)
  @inline def   ≤(l: D, r: D): Boolean = ! >(l, r)
}

object Geometric {
  implicit def wrap(v: Double): Geometric = new Geometric(1 + v)
  implicit def unwrap(t: Geometric): Double = t.ε
  implicit def show(implicit sd: Show[Double]): Show[Geometric] = Show.show { _.ε.show }

  type D = Double
  implicit class Ops(val l: D) extends AnyVal {
    @inline def >>=(r: D)(implicit ε: Geometric): Boolean = ε. >=(l, r)
    @inline def   ≥(r: D)(implicit ε: Geometric): Boolean = ε.  ≥(l, r)
    @inline def >>>(r: D)(implicit ε: Geometric): Boolean = ε.  >(l, r)
    @inline def <<=(r: D)(implicit ε: Geometric): Boolean = ε. <=(l, r)
    @inline def   ≤(r: D)(implicit ε: Geometric): Boolean = ε.  ≤(l, r)
    @inline def <<<(r: D)(implicit ε: Geometric): Boolean = ε.  <(l, r)
    @inline def ===(r: D)(implicit ε: Geometric): Boolean = ε.===(l, r)
    @inline def !==(r: D)(implicit ε: Geometric): Boolean = ε.!==(l, r)
    @inline def  <>(r: D)(implicit ε: Geometric): Boolean = ε. <>(l, r)
  }

  trait HasGeometric {
    type E = Geometric
    implicit val GeometricOps = Ops _
  }
}
