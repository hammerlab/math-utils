package org.hammerlab.collection

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * [[CanBuildFrom]] instances for constructing [[Array]]s and [[Vector]]s, not provided in standard library for some
 * reason.
 */
trait CanBuildFroms {

  trait SimpleCanBuildFrom[-From, -Elem, +To] extends CanBuildFrom[From, Elem, To] {
    @inline override def apply(from: From): mutable.Builder[Elem, To] = apply()
  }

  implicit def canBuildArray[From, T: ClassTag] =
    new SimpleCanBuildFrom[From, T, Array[T]] {
      def apply(): mutable.Builder[T, Array[T]] = mutable.ArrayBuilder.make[T]
    }

  implicit def canBuildVector[From, T] =
    new SimpleCanBuildFrom[From, T, Vector[T]] {
      def apply(): mutable.Builder[T, Vector[T]] = Vector.newBuilder[T]
    }

  implicit def canBuildStream[From, T] =
    new SimpleCanBuildFrom[From, T, Stream[T]] {
      def apply(): mutable.Builder[T, Stream[T]] = Stream.newBuilder[T]
    }

  implicit def canBuildList[From, To]: CanBuildFrom[From, To, List[To]] =
    new SimpleCanBuildFrom[From, To, List[To]] {
      def apply(): mutable.Builder[To, List[To]] = List.newBuilder
    }
}
