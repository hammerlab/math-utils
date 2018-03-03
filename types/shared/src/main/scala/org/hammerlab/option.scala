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
}
