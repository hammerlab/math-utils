package cubic.complex

import org.hammerlab.math.syntax.{ Arithmetic, Doubleish, E }
import spire.algebra._
import spire.math._

import scala.math.Pi

abstract class Cubic[CoeffT: Field, ResultT](implicit ε: E) {

//  def diff(a: ResultT, b: CoeffT): ResultT

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
  implicit def doubleComplex[D: Ordering: Field: NRoot: Trig : IsReal : Doubleish : Signed /*: Arithmetic.D : Math*/](implicit ε: E) =
    new Cubic[D, Complex[D]] {

      override def monic(b: D, c: D, d: D): Seq[Complex[D]] = {
        val b3   = b / 3
        val b32  = b3 * b3

        val q = b3*b32*2 - b3*c + d

        import org.hammerlab.math.syntax.FuzzyCmp._

        (
          if (c === 3*b32) {
            if (2*b3*b32 - b3*c === -d) {
              Seq(
                // TODO: express these in terms of the coefficients, for more numerical stability, gradient propagation,
                // etc.
                Complex(Field[D].fromDouble(0.0)),
                Complex(Field[D].fromDouble(0.0)),
                Complex(Field[D].fromDouble(0.0))
              )
            } else {
              val sqp32 = Complex(-q).nroot(3)
              val t0 = Complex(c - 3 * b32)
              val t1 = t0 - Field[D].fromDouble(pi23)
              val t2 = t1 - Field[D].fromDouble(pi23)

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
          _ - b3
        }
      }

      override def depressed(p: D, q: D): Seq[Complex[D]] = {
        val (sqp32, t0) =
          if (p == 0) {
            (
              Complex(-q).nroot(3),
              Complex(Field[D].fromDouble(0.0))
            )
          } else {
            val p3 = -p / 3
            val p33 = p3 * p3 * p3
            val q2 = -q / 2
            val q22 = q2 * q2

            val cos = Complex(abs[D](q2) / p3) / Complex(p3).nroot(2)
            val sqp3 = Complex(p3).nroot(2)
            val sqp32 = sqp3 * 2

            (
              if (q >= Field[D].fromDouble(0.0))
                -sqp32
              else
                sqp32,
              cos.acos / 3
            )
          }

        val t1 = t0 - pi23
        val t2 = t1 - pi23

        def root(t: Complex[D]) = sqp32 * t.cos

        Seq(
          root(t2),
          root(t1),
          root(t0)
        )
      }
    }

  val pi23 = 2 * Pi / 3
}
