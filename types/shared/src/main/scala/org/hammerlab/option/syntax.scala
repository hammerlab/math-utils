package org.hammerlab.option

trait syntax {
  @inline implicit def OptionBoolOps(b: Boolean) = Ops(b)

  trait dsl {
    // Symbolic shorthands
    type ?[+T] = Option[T]
     val ?     = Option
  }
  object dsl extends dsl

  object lift {
    // This is often convenient, to skip boilerplate around wrapping values with "Some(…)", but can interfere with e.g.
    // syntax that adds a `.map` method to an object, because [[Option.map]] will be discovered
    @inline implicit def liftOption[T](t: T): Option[T] = Some(t)
  }
}
object syntax extends syntax
