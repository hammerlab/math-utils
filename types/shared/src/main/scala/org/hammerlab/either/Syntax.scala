package org.hammerlab.either

import hammerlab.either._
import GetOrThrow.Ops

trait syntax {
  @inline implicit def EitherGetOrThrowOps[A](e: Throwable | A): Ops[A] = Ops(e)
}
object GetOrThrow {
  implicit class Ops[A](val e: Throwable | A) extends AnyVal {
    def getOrThrow: A = e.fold(throw _, a ⇒ a)
  }
}
