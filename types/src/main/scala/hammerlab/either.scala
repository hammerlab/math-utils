package hammerlab

trait either {
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
