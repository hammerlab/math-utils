package org.hammerlab.math

import shapeless._

/**
 * Copied/Adapted from
 * https://github.com/milessabin/shapeless/blob/shapeless-2.3.2/examples/src/main/scala/shapeless/examples/monoids.scala
 */

trait MonoidSyntax[T] {
  def |+|(b: T): T
}

object MonoidSyntax {
  implicit def monoidSyntax[T](a: T)(implicit mt: Monoid[T]): MonoidSyntax[T] =
    new MonoidSyntax[T] {
      def |+|(b: T) = mt.append(a, b)
    }
}

trait Monoid[T] {
  def zero: T
  def append(a: T, b: T): T
}

object Monoid extends ProductTypeClassCompanion[Monoid] {
  def zero[T](implicit mt: Monoid[T]) = mt.zero

  implicit def longMonoid: Monoid[Long] = new Monoid[Long] {
    def zero = 0
    def append(a: Long, b: Long) = a + b
  }

  implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def append(a: Int, b: Int): Int = a + b
  }

  implicit def stringMonoid: Monoid[String] = new Monoid[String] {
    override def zero: String = ""
    override def append(a: String, b: String): String = a + b
  }

  object typeClass extends ProductTypeClass[Monoid] {
    def emptyProduct = new Monoid[HNil] {
      def zero = HNil
      def append(a: HNil, b: HNil) = HNil
    }

    def product[F, T <: HList](mh: Monoid[F], mt: Monoid[T]) = new Monoid[F :: T] {
      def zero = mh.zero :: mt.zero
      def append(a: F :: T, b: F :: T) = mh.append(a.head, b.head) :: mt.append(a.tail, b.tail)
    }

    def project[F, G](instance: => Monoid[G], to: F => G, from: G => F) = new Monoid[F] {
      def zero = from(instance.zero)
      def append(a: F, b: F) = from(instance.append(to(a), to(b)))
    }
  }
}
