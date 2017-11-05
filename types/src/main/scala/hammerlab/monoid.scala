package hammerlab

import cats.instances.MapInstances
import cats.syntax.MonoidSyntax
import shapeless._

/**
 * Wildcard-import for convenient [[cats.Monoid]] things:
 *
 *   - auto-case-class-derivation
 *   - default instances for [[Int]], [[Long]], [[Map]]
 *   - `|+|` syntax
 */
object monoid
  extends CaseClassMonoids
    with MonoidInstances
    with MonoidSyntax

/**
 * Automatic [[cats.Monoid]] construction for case-classes, taken from
 * https://github.com/milessabin/shapeless/blob/shapeless-2.3.2/examples/src/main/scala/shapeless/examples/monoids.scala.
 */
trait CaseClassMonoids extends ProductTypeClassCompanion[cats.Monoid] {
  def zero[T](implicit mt: cats.Monoid[T]) = mt.empty

  object typeClass extends ProductTypeClass[cats.Monoid] {
    def emptyProduct = new cats.Monoid[HNil] {
      def empty = HNil
      def combine(a: HNil, b: HNil) = HNil
    }

    def product[F, T <: HList](mh: cats.Monoid[F], mt: cats.Monoid[T]) = new cats.Monoid[F :: T] {
      def empty = mh.empty :: mt.empty
      def combine(a: F :: T, b: F :: T) = mh.combine(a.head, b.head) :: mt.combine(a.tail, b.tail)
    }

    def project[F, G](instance: => cats.Monoid[G], to: F => G, from: G => F) = new cats.Monoid[F] {
      def empty = from(instance.empty)
      def combine(a: F, b: F) = from(instance.combine(to(a), to(b)))
    }
  }
}

trait MonoidInstances
  extends MapInstances {
  implicit val intMonoid = cats.implicits.catsKernelStdGroupForInt
  implicit val longMonoid = cats.implicits.catsKernelStdGroupForLong
}

object CaseClassMonoids extends CaseClassMonoids

