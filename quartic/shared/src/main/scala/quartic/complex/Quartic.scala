package quartic.complex

import cubic.complex.{ Cubic, DoubleComplex, DoubleResult }
import org.hammerlab.math.polynomial.Result
import org.hammerlab.math.syntax.Tolerance
import spire.algebra.Field
import spire.implicits._
import spire.math.Complex
import math.{abs, sqrt}
import org.hammerlab.math.syntax.FuzzyCmp._

abstract class Quartic[CoeffT: Field, ResultT](implicit ε: Tolerance) {
  def diff(a: ResultT, b: CoeffT): ResultT

  def apply(a: CoeffT, b: CoeffT, c: CoeffT, d: CoeffT, e: CoeffT): Seq[ResultT] =
    monic(
      b / a,
      c / a,
      d / a,
      e / a
    )

  def monic(b: CoeffT, c: CoeffT, d: CoeffT, e: CoeffT): Seq[ResultT]/* = {
    val b2 = -b/2
    val b4 = -b/4
    val b28 = b2 * b4

    val dc = c - b28 * 3
    val dd = b28*b + b2*c + d
    val de = e + b4*d + b28*c/2 - b28*b28*3/4

    depressed(dc, dd, de).map { diff(_, b4) }
  }
*/

  def depressed(c: CoeffT, d: CoeffT, e: CoeffT): Seq[ResultT]
}

object Quartic {
  type D = Double

  implicit def doubleComplex(implicit ε: Tolerance) =
    new Quartic[D, Complex[D]]
      with DoubleComplex {

      override def monic(b: D, c: D, d: D, e: D): Seq[Complex[D]] = {
        val b2 = -b/2
        val b4 = -b/4
        val b28 = b2 * b4

        val dc = c - b28 * 3
        val dd = b28*b + b2*c + d
        val de = e + b4*d + b28*c/2 - b28*b28*3/4

        val scale =
          Seq(
            abs(b)/4,
            sqrt(abs(c)/6),
            abs(d).pow(1.0/3),
            abs(e).pow(1.0/4)
          )
          .max

        def zero(v: D): Boolean = scale + v === scale

        println(s"\tmonic: b $b c $c d $d e $e, dc $dc dd $dd de $de scale: $scale")

        (
          if (zero(dd)) {
            if (zero(dc) && zero(de)) {
              println("\tquad")
              Seq[Complex[D]](0, 0, 0, 0)
            } else if (!zero(dc) && !zero(de)) {
              println("\tbiquad")
              biquad(dc, de)
            } else
              depressed(dc, dd, de)
          } else
            depressed(dc, dd, de)
        )
        .map { _ + b4 }
      }

      def biquad(c: D, e: D): Seq[Complex[D]] = {
        val c2 = c/2
        val d = Complex(c2*c2 - e).nroot(2)
        val r1 = (d - c2).nroot(2)
        val r2 = (-d - c2).nroot(2)
        Seq(
          -r1, r1, -r2, r2
        )
      }

      override def depressed(c: D, d: D, e: D): Seq[Complex[D]] = {
        val cubics =
          Cubic.doubleComplex.monic(
            2*c,
            c*c - 4*e,
            -d*d
          )

        println(s"\tcubics: $cubics")

        val u = cubics.find(_ != 0).get

        val squ2 = u.nroot(2) / 2
        val u2c = -u - c*2
        val du = d / squ2

        val n1 = u2c - du
        val n1s = -n1.nroot(2)

        val n2 = u2c + du
        val n2s = -n2.nroot(2)

        val s1 = (u2c - du).nroot(2) / 2

        val s2 = (u2c + du).nroot(2) / 2

        val roots =
          Seq(
             squ2 - s1,
             squ2 + s1,
            -squ2 - s2,
            -squ2 + s2
          )

        println(s"\tu: $u, $squ2 ± $s1, ${-squ2} ± $s2, squ2: $squ2, u2c $u2c du $du")
        println(s"\tdep roots: $roots")

        roots
      }
    }

  implicit def doubleResult(implicit ε: Tolerance) =
    new Quartic[D, Result[D]]
      with DoubleResult {
      override def     monic(b: D, c: D, d: D, e: D): Seq[Result[D]] = makeResults(doubleComplex.    monic(b, c, d, e))
      override def depressed(      c: D, d: D, e: D): Seq[Result[D]] = makeResults(doubleComplex.depressed(   c, d, e))
    }
}
