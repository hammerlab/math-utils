package org.hammerlab.ordering

import shapeless._

import scala.collection.immutable.SortedSet

/**
 * Taken from shapeless examples and lightly augmented
 */
trait LowPriorityGenericOrdering
  extends Serializable {
  implicit def caseClass[A, H <: HList](implicit
                                        gen: Generic.Aux[A, H],
                                        oh: Lazy[Ordering[H]]): Ordering[A] =
    new Ordering[A] {
      def compare(a1: A, a2: A) =
        oh
          .value
          .compare(
            gen to a1,
            gen to a2
          )
    }
}

trait generic
  extends LowPriorityGenericOrdering
    with Ordering.ExtraImplicits {

  def apply[T](implicit o: Ordering[T]): Ordering[T] = o

  implicit val hnil: Ordering[HNil] =
    new Ordering[HNil] {
      def compare(a: HNil, b: HNil) = 0
    }

  implicit def cons[H, T <: HList](implicit
                                   oh: Lazy[Ordering[H]],
                                   ot: Lazy[Ordering[T]]) : Ordering[H :: T] =
    new Ordering[H :: T] {
      def compare(a: H :: T, b: H :: T) = {
        val i = oh.value.compare(a.head, b.head)
        if (i == 0) ot.value.compare(a.tail, b.tail)
        else i
      }
    }

  implicit def iterator[T](implicit ord: Ordering[T]) =
    new Ordering[Iterator[T]] {
      override def compare(x: Iterator[T], y: Iterator[T]): Int = {
        (x.hasNext, y.hasNext) match {
          case (false, false) ⇒ 0
          case (true, true) ⇒
            ord.compare(x.next(), y.next()) match {
              case 0 ⇒ compare(x, y)
              case v ⇒ v
            }
          case (false, true) ⇒ -1
          case (true, false) ⇒ 1
        }
      }
    }

  implicit def arr[T: Ordering]: Ordering[Array[T]] = Ordering.by(_.iterator)
  implicit val range: Ordering[Range] = Ordering.by(_.iterator)
  implicit def sortedSet[T: Ordering]: Ordering[SortedSet[T]] = Ordering.by(_.iterator)
}

object generic extends generic
