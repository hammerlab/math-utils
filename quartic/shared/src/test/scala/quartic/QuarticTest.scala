package quartic

import org.hammerlab.Suite
import quartic.Root._

import scala.Array.fill

class QuarticTest
  extends Suite {

  type Dbl = scala.Double

  val M = 6
  val m = -M

  type R = Root[Int]

  def rootsIter =
    new Iterator[Seq[R]] {
      var _next: Option[Seq[R]] = _
      var advanced = false

      def popMaxs(roots: Seq[R]): Option[(Int, R, Seq[R])] = {
        var idx = roots.size - 1
        var degree = 0
        while (idx >= 0 && roots(idx).value == M) {
          degree += roots(idx).degree
          idx -= 1
        }

        if (idx < 0)
          None
        else {
          val last = roots(idx)
          val prefix = roots.view.slice(0, idx)
          Some(
            (
              degree,
              last,
              prefix
            )
          )
        }
      }

      def advance(): Unit =
        if (!advanced) {
          _next match {
            case null ⇒
              _next =
                Some(
                  Vector(
                    Quadruple(m)
                  )
                )
            case Some(prev) ⇒
              popMaxs(prev) match {
                case Some((degree, last, roots)) ⇒
                  _next =
                    Some(
                      roots ++
                        (
                          last.degree match {
                            case 1 ⇒
                              Seq(
                                Root(
                                  last. value + 1,
                                  last.degree + degree
                                )
                              )
                            case d ⇒
                              Seq(
                                Root(
                                  last.value,
                                  d - 1
                                ),
                                Root(
                                  last.value + 1,
                                  degree + 1
                                )
                              )
                          }
                        )
                    )
                case None ⇒
                  _next = None
              }
            case None ⇒
          }
          advanced = true
        }

      override def hasNext: Boolean = {
        advance()
        _next.isDefined
      }

      override def next(): Seq[R] = {
        advance()
        val r = _next.get
        advanced = false
        r
      }
    }

  test("roots sweep") {
    rootsIter
//      .take(10)
      .foreach {
        roots ⇒
          val raw =
            roots
              .flatMap {
                case Root(value, degree) ⇒
                  fill(degree)(value)
              }

          val Seq(b, c, d, e) =
            for {
              arity ← 1 to 4
            } yield
              raw
                .zipWithIndex
                .combinations(arity)
                .map {
                  _.map(_._1).product * (if (arity % 2 == 0) 1 else -1)
                }
                .sum

          println(s"roots: ${roots.mkString(" ")}\t coeffs: $b $c $d $e")
          withClue(s"roots: ${roots.mkString(" ")}\t coeffs: $b $c $d $e: ") {
            ===(
              Quartic.monic[Dbl](b, c, d, e),
              roots.map(map(_.toDouble))
            )
          }
      }
//    val min = -10
//    val max = 10
//    val Seq(b, c, d, e) =
//      for {
//        r1 ← min to max
//        r2 ←  r1 to max
//        r3 ←  r2 to max
//        r4 ←  r3 to max
//        roots = Seq(r1, r2, r3, r4)
//        c ← 1 to 4
//      } yield
//        roots
//          .sliding(c)
//          .zipWithIndex
//          .map {
//            case (group, idx) ⇒
//              group.product * (if (idx % 2 == 0) -1 else 1)
//          }
//
//    Quartic.monic(b, c, d, e)
  }

}
