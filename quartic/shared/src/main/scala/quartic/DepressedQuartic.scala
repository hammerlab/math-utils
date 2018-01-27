package quartic

import hammerlab.math.syntax.Arithmetic._
import hammerlab.math.syntax.FuzzyCmp._
import hammerlab.math.syntax.Math._
import hammerlab.math.syntax._
import cubic.Cubic
import quartic.Root._
import Doubleish.ord

object DepressedQuartic {
  def apply[
      D: Math
       : Arithmetic.I
       : Arithmetic.D
       : Doubleish
  ](
      a: D,
      b: D,
      c: D
  )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
  ):
    Seq[Root[D]] = {
//    println(s"\tdepressed: $a $b $c")
    val singleRoots =
      if (b === 0) {
        // Biquadratic case
        val d = a*a - c*4
//        println(s"\tbiquadratic: $d")
        if (d >= 0) {
          val sq = d.sqrt
          Seq(
            (-a + sq) / 2,
            (-a - sq) / 2
          )
          .filter(_ >= 0)
          .flatMap {
            d ⇒
              val sq = d.sqrt
              Seq(-sq, sq)
          }
          .sorted
        } else
          Nil
      } else {
        val cubics =
          Cubic.monic[D](
            2 * a,
            a*a - (4*c:D),
            -b*b
          )

//        println(s"\tcubics: $cubics")

        cubics
        .map(_.value)
        .find(d ⇒ !(d <= 0)) match {
          case Some(d) ⇒

//            println(s"\td: $d")

            val sqd2 = d.sqrt / 2
            val d2a = -d - (2 * a: D)
            val bd = b / sqd2
            val n1 = d2a - bd
            val n2 = d2a + bd
            def rootPair(sq: D, n: D) =
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
              rootPair( sqd2, n1) ++
              rootPair(-sqd2, n2)
            )
            .sorted
          case None ⇒
            throw new Exception(
              s"no resolvent cubic root found for $a $b $c"
            )
        }
      }

//    println(s"\tsingles: $singleRoots")

    val iter = singleRoots.iterator.buffered

    new Iterator[Root[D]] {
      override def hasNext: Boolean = iter.hasNext
      override def next(): Root[D] = {
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
