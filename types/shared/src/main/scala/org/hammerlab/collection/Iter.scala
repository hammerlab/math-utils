package org.hammerlab.collection

/**
 * Type-class for [[Iterable]]s, [[Iterator]]s, and [[Array]]s
 *
 * Mostly useful for downstream type-class implicit derivations, but exposes some direct syntax as well
 */
trait Iter[F[_]] {
  def   iter[T](f: F[T]): Iterator[T]
  def   list[T](f: F[T]):     List[T] = iter(f).toList
  def stream[T](f: F[T]):   Stream[T] = iter(f).toStream
  def    vec[T](f: F[T]):   Vector[T] = iter(f).toVector
}

object Iter {
  implicit val      seq: Iter[     Seq] = new Iter[     Seq] { def iter[T](x:      Seq[T]) = x.iterator }
  implicit val     list: Iter[    List] = new Iter[    List] { def iter[T](x:     List[T]) = x.iterator }
  implicit val    array: Iter[   Array] = new Iter[   Array] { def iter[T](x:    Array[T]) = x.iterator }
  implicit val   stream: Iter[  Stream] = new Iter[  Stream] { def iter[T](x:   Stream[T]) = x.iterator }
  implicit val   vector: Iter[  Vector] = new Iter[  Vector] { def iter[T](x:   Vector[T]) = x.iterator }
  implicit val iterable: Iter[Iterable] = new Iter[Iterable] { def iter[T](x: Iterable[T]) = x.iterator }
  implicit val iterator: Iter[Iterator] = new Iter[Iterator] { def iter[T](x: Iterator[T]) = x }

  implicit class Ops[F[_], T](val f: F[T]) extends AnyVal {
    def    vec(implicit F: Iter[F]):   Vector[T] = F.   vec(f)
    def   iter(implicit F: Iter[F]): Iterator[T] = F.  iter(f)
    def   list(implicit F: Iter[F]):     List[T] = F.  list(f)
    def stream(implicit F: Iter[F]):   Stream[T] = F.stream(f)
  }
  trait syntax {
    type Iter[F[_]] = org.hammerlab.collection.Iter[F]
    @inline implicit def makeIterOps[F[_], T](f: F[T]) = Ops(f)
  }
}
