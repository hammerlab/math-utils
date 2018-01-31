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
    Seq[Root[T]] = {
    println(s"\tdepressed: $c $d $e")
    val singleRoots =
      if (d === 0) {
        // Biquadratic case
        val c2 = -c/2
        val c22 = c2 * c2
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

              println(s"\td: $r")

              val sqr2 = r.sqrt / 2
              val r2a = -r - (2 * c: T)
              val bd = d / sqr2
              val n1 = r2a - bd
              val n2 = r2a + bd
              def rootPair(sq: T, n: T) =
                if (n >= 0) {
                  val s = n.sqrt / 2
                  Seq(
                    sq - s,
                    sq + s
                  )
                } else {
                  Nil
                }

              (
                rootPair( sqr2, n1) ++
                rootPair(-sqr2, n2)
              )
              .sorted
            case None ⇒
              throw new Exception(
                s"no resolvent cubic root found for $c $d $e"
              )
          }
      }

    println(s"\tsingles: $singleRoots")

    val iter = singleRoots.iterator.buffered

    new Iterator[Root[T]] {
      override def hasNext: Boolean = iter.hasNext
      override def next(): Root[T] = {
        val root = iter.next
        var last = root
        var n = 1
        var sum = root
        while (iter.hasNext && iter.head === last) {
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
}
