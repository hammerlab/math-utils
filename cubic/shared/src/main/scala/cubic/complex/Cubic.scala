package cubic.complex

import org.hammerlab.math.polynomial.{ ImaginaryRootPair, Real, Result }
import org.hammerlab.math.syntax.Tolerance
import spire.algebra._
import spire.math._
import spire.implicits._
import org.hammerlab.math.syntax.FuzzyCmp._
import Cubic.pi23

import math.Pi

abstract class Cubic[CoeffT: Field, ResultT](implicit ε: Tolerance) {

  def diff(a: ResultT, b: CoeffT): ResultT

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

  implicit def doubleComplex(implicit ε: Tolerance) =
    new Cubic[Double, Complex[Double]] {

      override def diff(a: Complex[D], b: D): Complex[D] = a - b

      override def monic(b: D, c: D, d: D): Seq[Complex[D]] = {
        val b3   = b / 3
        val b32  = b3 * b3

        val q = 2*b3*b32 - b3*c + d

        (
          if (c === 3*b32) {
            if (2*b3*b32 - b3*c === -d) {
//              println("triple")
              Seq(
                Complex(0.0),
                Complex(0.0),
                Complex(0.0)
              )
            } else {
//              println(s"short-circuit: p ${c - 3 * b32} q $q")
              val (sqp32, t0) = (Complex(-q).nroot(3), Complex(c - 3 * b32))
              val t1 = t0 - pi23
              val t2 = t1 - pi23

              def root(t: Complex[D]) = sqp32 * t.cos

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
//            println(s"\tundepressing: $r - $b3 == ${diff(r, b3)}")
            diff(r, b3)
        }
      }

      override def depressed(p: D, q: D): Seq[Complex[D]] = {
//        println(s"depressed: $p $q")
        val (sqp32, t0) =
          if (p == 0) {
            (Complex(-q).nroot(3), Complex(0.0))
          } else {
            val p3 = -p / 3
            val p33 = p3 * p3 * p3
            val q2 = -q / 2
            val q22 = q2 * q2

            //      println(s"\tq22: $q22, p33: $p33")

            val cos = abs(q2) / p3 / Complex(p3).sqrt
            val sqp3 = Complex(p3).sqrt
            val sqp32 = sqp3 * 2

//            println(s"\tdo acos: $cos (${cos.acos} ${cos.acos / 3})")
            val t0 = cos.acos / 3

            (if (q >= 0) -sqp32 else sqp32, t0)
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


  implicit def doubleResult(implicit ε: Tolerance) =
    new Cubic[Double, Result[D]] {

      override def diff(a: Result[D], b: D): Result[D] =
        a match {
          case Real(r) ⇒ Real(r - b)
          case ImaginaryRootPair(r, c) ⇒ ImaginaryRootPair(r - b, c)
        }

      def makeResults(complexes: Seq[Complex[D]]): Seq[Result[D]] = {
        val scale = complexes.flatMap { case Complex(a, b) ⇒ Seq(abs(a), abs(b)) }.max

        complexes
          .partition {
            case Complex(a, b) ⇒ scale + b === scale
          } match {
            case (reals, Nil) ⇒ reals.map(r ⇒ Real(r.real)).sortBy(_.t)
            case (Seq(r), Seq(Complex(a1, b1), Complex(a2, b2))) ⇒
              Seq(
                Real(r.real),
                ImaginaryRootPair(
                  (a1 + a2) / 2,
                  abs((b1 - b2) / 2)
                )
              )
          }
      }

      override def     monic(b: D, c: D, d: D): Seq[Result[D]] = makeResults(doubleComplex.    monic(b, c, d))
      override def depressed(      p: D, q: D): Seq[Result[D]] = makeResults(doubleComplex.depressed(   p, q))
    }

  type D = Double
  type Res = Complex[D]


  /*  import Root.map
    import hammerlab.iterator._
    implicit def doubleRoot(implicit ε: Tolerance) =
      new Cubic[Double, Root[Double]] {
        override def diff(a: Root[D], b: D): Root[D] = map[D, D](_ - b)(a)
        override def depressed(p: D, q: D): Seq[Root[D]] = {
          val iterator = doubleResult.depressed(p, q).iterator
          val reals =
            iterator
              .collectwhile {
                case r @ Real(_) ⇒ r
              }
              //.toVector


          import org.hammerlab.math.syntax.FuzzyCmp._
          def group(cur: Real[D], next: Real[D]): Option[Real[D]] =
            if (cur.t === next.t)
              Some(Real(cur.t + next.t))
            else
              None

          reals
            .runLengthEncode(group _)
            .map {
              case (Real(sum), n) ⇒ Root(sum / n, n)
            }
            .toVector ++
          iterator.toVector

        }
      }*/

  val pi23 = 2 * Pi / 3
}
