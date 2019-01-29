package hammerlab

import org.hammerlab.either.syntax

/**
 * Shorthands for [[Left]]/[[Right]]
 */
trait either
  extends syntax {
  type |[+L, +R] = Either[L, R]

  object L {
    def apply[A](a: A) = Left(a)
    def unapply[A](l: Left[A, _]): Option[A] = Some(l.a)
  }

  object R {
    def apply[B](b: B) = Right(b)
    def unapply[B](r: Right[_, B]): Option[B] = Some(r.b)
  }
}

object either extends either
