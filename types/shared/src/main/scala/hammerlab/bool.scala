package hammerlab

trait bool {

  /**
   * [[|]] syntax for wrapping a value in an option iff a boolean is true
   */
  implicit class BoolOps(val b: Boolean) extends Serializable {
    def |[A](a: ⇒ A): Option[A] =
      if (b)
        Some(a)
      else
        None
  }
}

object bool extends bool
