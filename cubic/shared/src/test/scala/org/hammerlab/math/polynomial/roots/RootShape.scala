package org.hammerlab.math.polynomial.roots

import hammerlab.show._
import Seq.fill

/**
 * Represents a number ([[reps]]) of roots of arity [[arity]]: one triple-root is (3,1) and three single-roots is (1,3).
 */
case class RootShape(arity: Int, reps: Int) {
  /* Total number of roots represented by this [[RootShape shape]] */
  val n = arity * reps
}

object RootShape {
  val delim = ","
  /* For display, by default, unroll all [[RootShape.reps repetitions]] of a given [[RootShape.arity arity]] */
  implicit val show: Show[RootShape] =
    Show {
      case RootShape(arity, reps) ⇒
        fill(reps)(arity).mkString(delim)
    }

  implicit val descendingArity: Ordering[RootShape] =
    Ordering.by(-_.arity)
}
