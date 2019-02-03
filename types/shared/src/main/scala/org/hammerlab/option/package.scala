package org.hammerlab

package object option {
  /**
   * [[?]] syntax for wrapping a value in an option iff a boolean is true
   */
  implicit class Ops(val b: Boolean)
    extends AnyVal {
    def ?[A](a: ⇒ A): Option[A] =
      if (b)
        Some(a)
      else
        None
  }
}
