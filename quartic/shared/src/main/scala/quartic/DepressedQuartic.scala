package quartic

import org.hammerlab.math.syntax.Arithmetic._
import org.hammerlab.math.syntax.FuzzyCmp._
import org.hammerlab.math.syntax.Math._
import org.hammerlab.math.syntax._
import cubic.Cubic
import quartic.Root._
import Doubleish.ord

object DepressedQuartic {
  def apply[
      T: Math
       : Arithmetic.I
       : Arithmetic.D
       : Doubleish
  ](
      c: T,
      d: T,
      e: T
  )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
  ):
    Seq[T] = {
    println(s"\tdepressed: $c $d $e")
    val singleRoots =
      if (d === 0) {
        // Biquadratic case
        biquadratic(c, e)
      } else {
        val cubics =
          Cubic.monic[T](
            2 * c,
            c * c - e *4,
            -d * d
          )

        println(s"\tcubics: $cubics")

        cubics
          .map(_.value)
          .find(_ >= 0) match {
            case Some(r) ⇒

              val sqr2 = r.sqrt / 2
              val r2a = -r - c*2
              val bd = d / sqr2

              val pair1 =
                if (r2a >= bd) {
                  val s = (r2a - bd).sqrt / 2
                  Seq(
                    sqr2 - s,
                    sqr2 + s
                  )
                } else
                  Nil

              val pair2 =
                if (r2a >= -bd) {
                  val s = (r2a + bd).sqrt / 2
                  Seq(
                    -sqr2 - s,
                    -sqr2 + s
                  )
                } else
                  Nil

              println(s"\tr: $r, p1: $pair1, p2: $pair2, sqr2: $sqr2, r2a $r2a bd $bd")

              (pair1 ++ pair2).sorted
            case None ⇒
              throw new Exception(
                s"no resolvent cubic root found for $c $d $e"
              )
          }
      }

//    println(s"\tsingles: $singleRoots")

    singleRoots
      //group(singleRoots)
  }

  def group[
      T: Doubleish
       : Arithmetic.I
       : Arithmetic.D
  ](
      singles: Seq[T]
  )(
      implicit
      ε: Tolerance
  ):
      Seq[Root[T]] =
    if (singles.isEmpty)
      Vector()
    else {
      import math.abs
      import Doubleish.DoubleishOps
      val M = singles.map(d ⇒ abs(d.toDouble)).max
      val iter = singles.iterator.buffered

      new Iterator[Root[T]] {
        override def hasNext: Boolean = iter.hasNext
        override def next(): Root[T] = {
          val root = iter.next
          var last = root
          var n = 1
          var sum = root
          println(s"dep quar root: $last")
          while (iter.hasNext && new FuzzyCmpOps(M).===(abs(iter.head.toDouble - last.toDouble) + M)) {
            n += 1
            last = iter.next
            sum += last
          }
          def avg = sum / n
          n match {
            case 1 ⇒    Single(root)
            case 2 ⇒    Double( avg)
            case 3 ⇒    Triple( avg)
            case 4 ⇒ Quadruple( avg)
          }
        }
      }
      .toVector
    }

  def biquadratic[
      T: Math
       : Arithmetic.I
       : Arithmetic.D
       : Doubleish
  ](
      c: T,
      e: T
   )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
   ):
      Seq[T] = {
    val c2 = -c/2
    val c22 = c2 * c2
    println(s"\tc2: $c2, c22: $c22, e: $e")
    (
      if (c22 === e) {
        Seq(
          c2,
          c2
        )
      } else if (c22 > e) {
        val sq = (c22 - e).sqrt
        println(s"\tsq: $sq")
        Seq(
          c2 - sq,
          c2 + sq
        )
      } else
        Nil
    )
    .filter(_ >= 0)
    .flatMap {
      d ⇒
        val sq = d.sqrt
        println(s"\td: $d, sq: $sq")
        Seq(-sq, sq)
    }
    .sorted
  }
}
