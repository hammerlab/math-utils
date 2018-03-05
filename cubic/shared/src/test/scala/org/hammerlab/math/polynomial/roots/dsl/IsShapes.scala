package org.hammerlab.math.polynomial.roots.dsl

import org.hammerlab.math.polynomial.roots.RootShapes.Shapes
import shapeless.ops.tuple.ToList

case class IsShapes[-T](f: T ⇒ Shapes) {
  def apply(t: T): Shapes = f(t)
}
object IsShapes {
  def make[T](f: T ⇒ Shapes): IsShapes[T] = IsShapes(f)
  def apply(reals: Seq[Int]): Shapes = Shapes(reals)
  implicit val int: IsShapes[Int] = make((n: Int) ⇒ apply(Seq(n)))
  implicit def fromReals[T](implicit
                            toList: ToList.Aux[T, Int, List[Int]]): IsShapes[T] =
    make(
      (t: T) ⇒
        apply(
          toList(t)
        )
    )
}
