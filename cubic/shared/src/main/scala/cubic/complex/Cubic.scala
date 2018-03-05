package cubic.complex

import org.hammerlab.math.FromDouble
import org.hammerlab.math.syntax.Doubleish
import spire.algebra._
import spire.math._

import scala.math.Pi

abstract class Cubic[Real: Field, Complex] {

  import spire.implicits._
  def apply(a: Real, b: Real, c: Real, d: Real): Seq[Complex] =
    monic(
      b / a,
      c / a,
      d / a
    )

  def quadratic(b: Real, c: Real): (Complex, Complex)

  def monic(b: Real, c: Real, d: Real): Seq[Complex]

  def depressed(p: Real, q: Real): Seq[Complex]
}

object Cubic {

  type RealToComplex[D] = Cubic[D, Complex[D]]

  import spire.implicits._
  implicit def doubleComplex[D: Ordering: FromDouble : Field: NRoot: Trig : IsReal : Doubleish : Signed] =
    new Cubic[D, Complex[D]] {

      override def monic(b: D, c: D, d: D): Seq[Complex[D]] = {
//        println(s"cub monic: $b $c $d")
        if (d.isZero) {
          val (r1, r2) = quadratic(b, c)
          Seq(Complex(d), r1, r2)
        } else {
          val b3   = b / 3
          val b32  = b3 * b3

          val p = c - 3*b32
          val q = b3*b32*2 - b3*c + d

          depressed(p, q)
            .map {
              r ⇒
  //              println(s"cub root: $r - $b3 = ${r - b3}")
                r - b3
            }
        }
      }

      def quadratic(b: D, c: D): (Complex[D], Complex[D]) =
        if (c.isZero)
          (
            Complex(c),
            Complex(-b)
          )
        else {
          val b2 = -b/2
          val sq = Complex(b2*b2 - c).nroot(2)
          val b2c = Complex(b2)
          (
            b2c - sq,
            b2c + sq
          )
        }

      override def depressed(p: D, q: D): Seq[Complex[D]] = {
//        println(s"cub depressed: $p $q, ε $ε")
        val (sqp32, t0) =
          if (p == 0) {
            (
              Complex(-q).nroot(3),
              Complex(FromDouble(0))
            )
          } else {
            val p3 = -p / 3
            val p33 = p3 * p3 * p3
            val q2 = -q / 2
            val q22 = q2 * q2

            val sqp3 = Complex(p3).nroot(2)
            val cos = Complex(abs(q2) / p3) / sqp3
            val sqp32 = sqp3 * 2

//            println(s"p3 $p3")
//            println(s"sqp3 $sqp3")

//            println(s"p33 $p33")
//            println(s"q22 $q22")

//            println(s"cos $cos")

            (
              if (q >= FromDouble(0))
                -sqp32
              else
                sqp32,
              cos.acos / 3
            )
          }

//        println(s"t0 $t0")
//        println(s"sqp32 $sqp32")

        val t1 = t0 - pi23
        val t2 = t1 - pi23

        def root(t: Complex[D]) = {
//          println(s"make root: $t")
          sqp32 * t.cos
        }

//        println("return roots")
        Seq(
          root(t2),
          root(t1),
          root(t0)
        )
      }

      val pi23 = 2 * Pi / 3
    }
}
