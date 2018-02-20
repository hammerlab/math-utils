package org.hammerlab.math.polynomial.result

import hammerlab.show._
import hammerlab.lines._
import spire.algebra.Field
import spire.implicits._
import spire.math.Complex

case class ResultGroup[D](head: Result[D], num: Int = 1)
object ResultGroup {
  implicit def liftResult[D](result: Result[D]  ): ResultGroup[D] = ResultGroup(result)
  implicit def liftTuple [D](t: (Result[D], Int)): ResultGroup[D] = ResultGroup(t._1, t._2)

  implicit def showComplex[D: Show : Field : Ordering]: Show[Complex[D]] =
    Show {
      case Complex(r, i) ⇒
        val f = Field[D]
        import f.{zero, one}
        val lt = Ordering[D].lt _
        val negativeOne = -one

        val rs =
          if (r == zero)
            ""
          else
            r.show

             if ( i ==  zero) r.show
        else if ( i ==   one)
               if (r == zero)
                 "i"
               else
                 s"$rs+i"
        else if (-i ==   one) s"$rs-i"
        else if (lt(i, zero)) show"$rs${i}i"
        else                  show"$rs+${i}i"
    }

  implicit def showResultGroup[D: Show : Field : Ordering](implicit d: Show[Double]): ToLines[ResultGroup[D]] =
    ToLines {
      case ResultGroup(result, num) ⇒ showResultGroupOpt[D].apply((result, Some(num)))
    }

  implicit def showResultGroupOpt[D: Show : Field : Ordering](implicit d: Show[Double]): ToLines[(Result[D], Option[Int])] =
    ToLines {
      case (Result(tc, actual, maxAbsErr, maxErrRatio, zeros), numOpt) ⇒

        val parenthetical =
          (
            (
              if (zeros.nonEmpty)
                zeros.show :: Nil
              else
                Nil
            ) ++ (
              numOpt
                .map {
                  num ⇒
                    show"$num copies"
                }
                .toList
            ) match {
              case Nil ⇒ None
              case e :: Nil ⇒ Some(e)
              case es ⇒ Some(es.mkString("; "))
            }
          )
          .map { s ⇒ s" ($s)"}
          .getOrElse("")

        val scaleStr =
          if (tc.scale == 1)
            ""
          else
           show", scale ${tc.scale}"

        Lines(
          show"max errs: abs: $maxAbsErr, ratio: ${maxErrRatio.fold("NaN")(_.show)}$scaleStr$parenthetical",
          indent(
            {
              val strs =
                tc
                  .roots
                  .zip(actual)
                  .map { case (e, a) ⇒ (e.show, a.show) }

              val maxE = strs.map(_._1.length).max
              val maxA = strs.map(_._2.length).max
              strs
                .map {
                case (e, a) ⇒
                  s"%${maxE}s    %${maxA}s".format(e, a)
              }
            }
          )
        )
    }
}

