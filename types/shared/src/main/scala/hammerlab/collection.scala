package hammerlab

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * [[CanBuildFrom]] instances for constructing [[Array]]s and [[Vector]]s, not provided in standard library for some
 * reason.
 */
trait collection {
  implicit def canBuildArray[From, T: ClassTag] =
    new CanBuildFrom[From, T, Array[T]] {
      def apply(from: From): mutable.Builder[T, Array[T]] = mutable.ArrayBuilder.make[T]
      def apply(          ): mutable.Builder[T, Array[T]] = mutable.ArrayBuilder.make[T]
    }

  implicit def canBuildVector[From, T] =
    new CanBuildFrom[From, T, Vector[T]] {
      def apply(from: From): mutable.Builder[T, Vector[T]] = Vector.newBuilder[T]
      def apply(          ): mutable.Builder[T, Vector[T]] = Vector.newBuilder[T]
    }

  implicit def canBuildList[From, To]: CanBuildFrom[From, To, List[To]] =
    new CanBuildFrom[From, To, List[To]] {
      def apply(from: From): mutable.Builder[To, List[To]] = List.newBuilder
      def apply(          ): mutable.Builder[To, List[To]] = List.newBuilder
    }
}

object collection extends collection
