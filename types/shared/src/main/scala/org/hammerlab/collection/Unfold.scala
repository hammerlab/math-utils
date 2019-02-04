package org.hammerlab.collection

import hammerlab.option.?

object Unfold {
  implicit class Ops[T](val t: T) extends AnyVal {
    def unfoldLeft[T2](fn: T ⇒ ?[(T2, T)]): Stream[T2] =
      fn(t) match {
        case None ⇒ Stream()
        case Some((result, next)) ⇒ result #:: next.unfoldLeft(fn)
      }

    def unfold(fn: T ⇒ ?[T]): Stream[T] =
      t #:: {
        fn(t) match {
          case None ⇒ Stream()
          case Some(next) ⇒ next.unfold(fn)
        }
      }
  }

  trait syntax {
    @inline implicit def makeUnfoldOps[T](t: T) = Ops(t)
  }
}
