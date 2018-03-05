package org.hammerlab.math.polynomial.roots.dsl

import org.hammerlab.math.polynomial.roots.RootShapes
import org.hammerlab.math.polynomial.roots.RootShapes.Shapes

case class IsRootShapes[-T](f: T ⇒ RootShapes) {
  def apply(t: T): RootShapes = f(t)
}
object IsRootShapes {
  implicit def fromShapes[T](implicit s: IsShapes[T]): IsRootShapes[T] =
    IsRootShapes(
      (t: T) ⇒
        RootShapes(s(t), Shapes(Nil))
    )
  trait HasOps {

    def ||[T](t: T)(implicit shapes: IsShapes[T]): RootShapes =
      RootShapes(
        Shapes(Nil),
        shapes(t)
      )

    implicit class RootShapeOps[L](l: L) {
      def ||[R](r: R)(implicit ll: IsShapes[L], rr: IsShapes[R]): RootShapes =
        RootShapes(
          ll(l),
          rr(r)
        )
    }
  }
}
