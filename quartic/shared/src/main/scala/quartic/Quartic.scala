package quartic

import org.hammerlab.math.syntax._
import org.hammerlab.math.syntax.Arithmetic._
import org.hammerlab.math.syntax.Math._
import FuzzyCmp._
import quartic.Root.Quadruple

import math.{ abs, max }

object Quartic {
  def apply[
      D: Math
       : Arithmetic.I
       : Arithmetic.D
       : Doubleish
  ](
      a: D,
      b: D,
      c: D,
      d: D,
      e: D
   )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
   ):
  Seq[Root[D]] =
    monic(
      b / a,
      c / a,
      d / a,
      e / a
    )

  def monic[
      D: Math
       : Arithmetic.I
       : Arithmetic.D
       : Doubleish
  ](
      b: D,
      c: D,
      d: D,
      e: D
   )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
   ):
  Seq[Root[D]] = {
    val b2 = -b/2
    val b4 = -b/4
    val b28 = b2 * b4

    val dc = c - b28 * 3
    val dd = b28*b + b2*c + d
    val de = e + b4*d + b28*c/2 - b28*b28*3/4

    import Doubleish.DoubleishOps

    val (sb, sc, sd, se) =
      (
        abs(b.toDouble / 4),
        abs(c.toDouble / 6).sqrt,
        abs((d.toDouble / 4).cbrt),
        abs(e.toDouble) ^ (1.0/4)
      )

    println(s"\tmonic:\t$b $c $d $e\n\t\tdc $dc dd $dd de $de\n\t\tsb $sb sc $sc sd $sd se $se")

    val scale =
      Seq(
        sb, sc, sd, se
//        abs(dc.toDouble),
        //abs(dd),
//        abs(de.toDouble)
//        abs(b.toDouble),
//        abs(c.toDouble),
//        abs(d.toDouble),
//        abs(e.toDouble)
      )
      .max

    def zero(d: D): Boolean = (abs(d.toDouble) + scale === scale)
    val bs = b28*b + b2*c
//    println(s"bs: $bs, -d: ${-d}")
    DepressedQuartic.group(
      (
        if (
          zero(dc) &&
          (
            zero(dd) ||
            abs(dd.toDouble) <= abs(dc.toDouble)
          ) &&
          (
            zero(de) ||
            abs(de.toDouble) <= abs(dd.toDouble)
          )
        ) {
          val r = (dc + dd + de) / 3
          println(s"\tquad: $r")
          Seq(r, r, r, r)
        }
        else if (zero(dd) && !zero(de)) {
          println(s"\tgoing biquad: $dc $dd $de")
          DepressedQuartic.biquadratic(dc, de)
        } else
          DepressedQuartic(
            dc,
            bs + d,
            de
          )
      )
      .map {
        r ⇒
          println(s"depressed root: $r")
          r + b4
      }
    )
  }
}
