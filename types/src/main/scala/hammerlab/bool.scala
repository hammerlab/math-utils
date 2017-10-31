package hammerlab

trait bool {
  implicit class BoolOps(val b: Boolean) extends Serializable {
    def |[A](a: ⇒ A): Option[A] =
      if (b)
        Some(a)
      else
        None
  }
}

object bool extends bool
