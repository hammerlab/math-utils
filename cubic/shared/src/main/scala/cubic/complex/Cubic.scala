package cubic.complex

import java.math.MathContext.DECIMAL128

import org.hammerlab.math.syntax.{ Arithmetic, Doubleish, E }
import spire.algebra._
import spire.math._

import scala.math.Pi

trait FromDouble[T] {
  def apply(d: Double): T
}
object FromDouble {
  def apply[T](implicit f: FromDouble[T]): FromDouble[T] = f
  def instance[T](fn: Double ⇒ T): FromDouble[T] =
    new FromDouble[T] {
      override def apply(d: Double): T = fn(d)
    }

  implicit val double: FromDouble[Double] = instance(x ⇒ x)
  implicit val bigDecimal: FromDouble[BigDecimal] =
    instance(
      BigDecimal(
        _,
        DECIMAL128
      )
    )
}

abstract class Cubic[CoeffT: Field, ResultT](implicit ε: E) {

  import spire.implicits._
  def apply(a: CoeffT, b: CoeffT, c: CoeffT, d: CoeffT): Seq[ResultT] =
    monic(
      b / a,
      c / a,
      d / a
    )

  def monic(b: CoeffT, c: CoeffT, d: CoeffT): Seq[ResultT]

  def depressed(p: CoeffT, q: CoeffT): Seq[ResultT]
}

object Cubic {

  import spire.implicits._
  implicit def doubleComplex[D: Ordering: FromDouble : Field: NRoot: Trig : IsReal : Doubleish : Signed /*: Arithmetic.D : Math*/](implicit ε: E) =
    new Cubic[D, Complex[D]] {

      override def monic(b: D, c: D, d: D): Seq[Complex[D]] = {
//        println(s"cub monic: $b $c $d")
        val b3   = b / 3
        val b32  = b3 * b3

        val q = b3*b32*2 - b3*c + d

        import org.hammerlab.math.syntax.FuzzyCmp._

        (
          if (c === 3*b32) {
            if (2*b3*b32 - b3*c === -d) {
//              println("getting z")
              val z = Complex(FromDouble[D].apply(0))
//              println(s"z: $z")
              Seq(
                // TODO: express these in terms of the coefficients, for more numerical stability, gradient propagation,
                // etc.
                z, z, z
              )
            } else {
//              println("3")
              val sqp32 = Complex(-q).nroot(3)
              val t0 = Complex(c - 3 * b32)
              val t1 = t0 - pi23
              val t2 = t1 - pi23

              def root(t: Complex[D]): Complex[D] = sqp32 * t.cos

              Seq(
                root(t2),
                root(t1),
                root(t0)
              )
            }

          } else {
            val p = c - 3*b32

            depressed(p, q)
          }
        )
        .map {
          r ⇒
//            println(s"root: $r")
            r - b3
        }
      }

      override def depressed(p: D, q: D): Seq[Complex[D]] = {
//        println(s"cub depressed: $p $q")
        val (sqp32, t0) =
          if (p == 0) {
            (
              Complex(-q).nroot(3),
              Complex(FromDouble[D].apply(0))
            )
          } else {
            val p3 = -p / 3
            val p33 = p3 * p3 * p3
            val q2 = -q / 2
            val q22 = q2 * q2

            val cos = Complex(abs(q2) / p3) / Complex(p3).nroot(2)
            val sqp3 = Complex(p3).nroot(2)
            val sqp32 = sqp3 * 2

//            println("acos…")
            (
              if (q >= FromDouble[D].apply(0))
                -sqp32
              else
                sqp32,
              cos.acos / 3
            )
          }

//        println("after else")
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
