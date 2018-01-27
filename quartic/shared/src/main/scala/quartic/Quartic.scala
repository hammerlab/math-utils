package quartic

import hammerlab.math.syntax._
import hammerlab.math.syntax.Arithmetic._
import hammerlab.math.syntax.Math._

object Quartic {
  def apply[
      D: Math
       : Arithmetic.I
       : Arithmetic.D
       : Doubleish
  ](
      a: D,
      b: D,
      c: D,
      d: D,
      e: D
   )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
   ):
  Seq[Root[D]] =
    monic(
      b / a,
      c / a,
      d / a,
      e / a
    )

  def monic[
      D: Math
       : Arithmetic.I
       : Arithmetic.D
       : Doubleish
  ](
      b: D,
      c: D,
      d: D,
      e: D
   )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
   ):
  Seq[Root[D]] = {
//    println(s"\tmonic: $b $c $d $e")
    val b2 = -b/2
    val b4 = -b/4
    val b28 = b2 * b4
    DepressedQuartic(
      c - (3 * b28: D),
      b28*b + b2*c + d,
      e + b4*d + b28*c/2 - b28*b28*3/4
    ).map {
      r ⇒
//        println(s"depressed root: $r")
        r + b4
    }
  }
}
