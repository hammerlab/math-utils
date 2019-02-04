package org.hammerlab.option

import org.hammerlab.option.syntax._

trait syntax {
  @inline implicit def OptionBoolOps(b: Boolean) = Ops(b)

  object lift {
    // This is often convenient, to skip boilerplate around wrapping values with "Some(…)", but can interfere with e.g.
    // syntax that adds a `.map` method to an object, because [[Option.map]] will be discovered
    @inline implicit def liftOption[T](t: T): Option[T] = Some(t)
  }
}
object syntax {
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

  trait dsl {
    // Symbolic shorthands
    type ?[+T] = Option[T]
    val ?     = Option
  }
}
