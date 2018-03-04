package quartic.complex

import cubic.complex.Cubic
import hammerlab.math.tolerance._
import org.hammerlab.math.FromDouble
import org.hammerlab.math.syntax.Doubleish
import org.hammerlab.math.syntax.FuzzyCmp._
import spire.algebra.{ Field, IsReal, NRoot, Signed, Trig }
import spire.implicits._
import spire.math.{ Complex, abs, sqrt }

abstract class Quartic[CoeffT: Field, ResultT](implicit ε: E) {
  def apply(a: CoeffT, b: CoeffT, c: CoeffT, d: CoeffT, e: CoeffT): Seq[ResultT] =
    monic(
      b / a,
      c / a,
      d / a,
      e / a
    )

  def monic(b: CoeffT, c: CoeffT, d: CoeffT, e: CoeffT): Seq[ResultT]

  def depressed(c: CoeffT, d: CoeffT, e: CoeffT): Seq[ResultT]
}

object Quartic {

  trait HasQuartic[D] {
    def apply(implicit ε: E): RealComplex[D]
  }
  object HasQuartic {
    implicit def forType[D: Doubleish : FromDouble : Field : NRoot : Signed : Trig : Ordering : IsReal]: HasQuartic[D] =
      new HasQuartic[D] {
        override def apply(implicit ε: E): RealComplex[D] = doubleComplex[D]
      }
  }

  type RealComplex[D] = Quartic[D, Complex[D]]

  implicit def doubleComplex[D: Doubleish : FromDouble : Field : NRoot : Signed : Trig : Ordering : IsReal](implicit ε: E) =
    new Quartic[D, Complex[D]] {

      override def monic(b: D, c: D, d: D, e: D): Seq[Complex[D]] = {
        if (e.isZero) {
          Complex(e) ::
          Cubic.doubleComplex.monic(
            b,
            c,
            d
          )
          .toList
        } else {
          val b2 = -b/2
          val b4 = -b/4
          val b28 = b2 * b4

          val dc = c - b28 * 3
          val dd = b28*b + b2*c + d
          val de = e + b4*d + b28*c/2 - b28*b28*3/4

//          println(s"monic: $b $c $d $e, $dc $dd $de")

          val scale =
            Seq(
              abs(b)/4,
              sqrt(abs(c)/6),
              abs(d).nroot(3),
              abs(e).nroot(4)
            )
            .max

          def zero(v: D): Boolean = scale + v === scale

          (
            if (zero(dd)) {
              if (zero(dc) && zero(de)) {
  //              println("quad")
                val z = Complex(dc + dd + de) / 3
                // TODO: express these in terms of the coefficients, for more numerical stability, gradient propagation,
                // etc.
                Seq(z, z, z, z)
              } else if (!zero(dc) && !zero(de)) {
//                println("biquad")
                biquad(dc, de)
              } else
                depressed(dc, dd, de)
            } else
              depressed(dc, dd, de)
          )
          .map { _ + b4 }
        }
      }

      private def biquad(c: D, e: D): Seq[Complex[D]] = {
        val (r1, r2) = Cubic.doubleComplex[D].quadratic(c, e)
        val (s1, s2) = (r1.sqrt, r2.sqrt)
        Seq(
          -s1, s1, -s2, s2
        )
      }

      override def depressed(c: D, d: D, e: D): Seq[Complex[D]] = {
//        println(s"depressed: $c $d $e")
        if (d == 0)
          biquad(c, e)
        else {
          val cubics =
            Cubic.doubleComplex.monic(
              2*c,
              c*c - 4*e,
              -d*d
            )

          val u = cubics.find(_ != 0).get

//          println(s"cubics:\n\t${cubics.mkString("\n\t")}")

          val two = FromDouble(2)
          val squ2 = u.nroot(2) / two
          val u2c = -u - c*2
          val du = Complex(d) / squ2

          val n1 = u2c - du
          val n1s = -n1.nroot(2)

          val n2 = u2c + du
          val n2s = -n2.nroot(2)

          val s1 = (u2c - du).nroot(2) / two
          val s2 = (u2c + du).nroot(2) / two

//          println(s"squ2 $squ2 u2c $u2c du $du two $two s1 $s1 s2 $s2")

          Seq(
             squ2 - s1,
             squ2 + s1,
            -squ2 - s2,
            -squ2 + s2
          )
        }
      }
    }
}
