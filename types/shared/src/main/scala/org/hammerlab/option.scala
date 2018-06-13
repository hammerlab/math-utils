package org.hammerlab

object option {
  /**
   * [[?]] syntax for wrapping a value in an option iff a boolean is true
   */
  implicit class BoolOps(val b: Boolean)
    extends AnyVal {
    def ?[A](a: ⇒ A): Option[A] =
      if (b)
        Some(a)
      else
        None
  }
}

trait HasOption {
  @inline implicit def OptionBoolOps(b: Boolean) = option.BoolOps(b)

  /**
   * Wrapper around [[Option]] that implicitly lifts unwrapped values into a [[Som]] (analogous to [[Some]])
   *
   * Useful for allowing
   */
  sealed trait Opt[+T]
  case class Som[+T](t: T) extends Opt[T]
  case object Non extends Opt[Nothing]
  object Opt {
    implicit def toOpt[T](o: Opt[T]): Option[T] =
      o match {
        case Som(t) ⇒ Some(t)
        case Non    ⇒ None
      }

    implicit def fromOpt[T](o: Option[T]): Opt[T] =
      o match {
        case Some(t) ⇒ Som(t)
        case None    ⇒ Non
      }

    implicit def lift[T](t: T): Opt[T] = Som(t)
  }
}
