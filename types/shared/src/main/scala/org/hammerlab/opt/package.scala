package org.hammerlab

package object opt {
  /**
   * Wrapper around [[Option]] that implicitly lifts unwrapped values into a [[Som]] (analogous to [[Some]])
   *
   * Useful for allowing
   */
  sealed trait Opt[+T] {
    def getOrElse[U >: T](implicit t: U): U =
      this match {
        case Som(t) ⇒ t
        case Non ⇒ t
      }
  }
  case class Som[+T](t: T) extends Opt[T]
  object Som {
    implicit def lift[T](t: T): Som[T] = Som(t)
  }
  case object Non extends Opt[Nothing]
  object Opt {
    def apply[T](t: T): Opt[T] = Som(t)
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

  trait syntax {
    // This mix-in puts the main classes above in scope, by their basenames
    trait std {
      type Opt[+T] = opt.Opt[T]
       val Opt     = opt.Opt
      type Som[+T] = opt.Som[T]
       val Som     = opt.Som
      type Non     = opt.Non.type
       val Non     = opt.Non
    }
    object std extends std

    trait dsl {
      type ?[+T] = Opt[T]
       val ?     = Opt
    }
    object dsl extends dsl

    trait stdlib {
      // Aliases that override the stdlib versions
      type Option[+T] = Opt[T]
       val Option     = Opt
      type   Some[+T] = Som[T]
       val   Some     = Som
      type   None     = Non.type
       val   None     = Non
    }
    object stdlib extends stdlib
  }
  object syntax extends syntax
}
