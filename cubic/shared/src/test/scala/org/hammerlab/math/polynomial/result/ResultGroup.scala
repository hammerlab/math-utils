package org.hammerlab.math.polynomial.result

import hammerlab.show._
import org.hammerlab.math.polynomial.result.Result.Root
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
        import f.{ one, zero }
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

  implicit def showResultGroup[D: Show : Field : Ordering](implicit d: Show[Double]): Show[ResultGroup[D]] =
    Show {
      case ResultGroup(result, num) ⇒
        showResultGroupOpt[D].apply(
          (
            result,
            Some(num)
          )
        )
    }

  implicit def showResultGroupOpt[D: Show : Field : Ordering](implicit d: Show[Double]): Show[(Result[D], Option[Int])] =
    Show {
      case (Result(tc, actual, err), numOpt) ⇒

        val parenthetical =
          numOpt
            .map {
              num ⇒
                show" ($num copies)"
            }
            .getOrElse("")

        val scaleStr =
          if (tc.scale == 1)
            ""
          else
           show", scale ${tc.scale}"

        val roots =
          actual
            .map {
              case Root(e, a, err) ⇒
                show"$e ⟶ $a ($err)"
            }
            .mkString(", ")

        show"err: $err$scaleStr$parenthetical, $roots"
    }
}

