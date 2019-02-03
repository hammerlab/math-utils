package hammerlab

import cats.data.Ior

/** Syntactic sugars around [[cats.data.Ior]]s */
trait or {
  type Or[+A, +B] = Ior[A, B]
  type ||[+A, +B] = Ior[A, B]

  object L {
    def apply[A](a: A) = Ior.Left(a)
    def unapply[A](l: Ior.Left[A]): Option[A] = Some(l.a)
  }

  object R {
    def apply[B](b: B) = Ior.Right(b)
    def unapply[B](r: Ior.Right[B]): Option[B] = Some(r.b)
  }

  object Both {
    def apply[A, B](a: A, b: B) = Ior.Both(a, b)
    def unapply[A, B](b: Ior.Both[A, B]): Option[(A, B)] = Some((b.a, b.b))
  }

  val B = Both
}

object or extends or
