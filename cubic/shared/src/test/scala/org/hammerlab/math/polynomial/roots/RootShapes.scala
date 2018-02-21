package org.hammerlab.math.polynomial.roots

import hammerlab.iterator._
import hammerlab.show._
import org.hammerlab.math.polynomial.ImaginaryRootPair
import org.hammerlab.math.polynomial.roots.RootShape.delim
import org.hammerlab.math.polynomial.roots.RootShapes.Shapes

import scala.collection.immutable.SortedSet

/**
 * Represents the number and arities of the roots (real and imaginary) of a polynomial
 *
 * For example, a cubic polynomial (with real coefficients) has four possible [[RootShapes]]:
 *
 * - one triple real root
 * - one double real root and one single real root
 * - three single real roots
 * - one single real root and one conjugate-pair of imaginary roots
 *
 * These are represented via a default [[cats.Show.show show]] as: (3), (2,1), (1,1,1), and (1)[1], respectively, and
 * [[Ordering ordered]] that way as well.
 */
case class RootShapes(reals: Shapes,
                      imags: Shapes) {
  val n =
    reals.map(_.n    ).sum +
    imags.map(_.n * 2).sum
}

object RootShapes {

  /**
   * Thin container for the real- and imaginary-[[RootShape]]s comprising a [[RootShapes]]
   *
   * Contained [[RootShape]]s are stored in descending order
   */
  case class Shapes(s: Shapes.T)
  object Shapes {
    type T = SortedSet[RootShape]
    implicit def unwrap(s: Shapes): Shapes.T = s.s
    implicit def apply(arities: Seq[Int]): Shapes =
      Shapes(
        SortedSet(
          arities
          .runLengthEncode
          .map {
            case (arity, num) ⇒
              RootShape(arity, num)
          }
          .toList: _*
        )
      )
    implicit val showShapeSet: Show[Shapes] =
      Show {
        _
          .iterator
          .map(_.show)
          .mkString(delim)
      }
  }

  /**
   * Generate all [[RootShapes]] for a polynomial of degree `n` with real coefficients
   *
   * Iterator over the possible number of [[ImaginaryRootPair]]s (from 0 to `n/2`) and partition the corresponding
   * numbers of real and imaginary roots over the possible shapes comprising that number of total roots
   */
  def apply(n: Int): Iterator[RootShapes] =
    for {
      numImagPairs ← (0 to n/2).iterator
      imags ←
        numImagPairs
          .unorderedPartitions
          .map(Shapes(_))

      numReals = n - 2*numImagPairs
      reals ←
        numReals
          .unorderedPartitions
          .map(Shapes(_))
    } yield
      RootShapes(
        reals,
        imags
      )

  /**
   * Display real-root-shapes in parends, followed by imaginary-root-pair-shapes in brackets, omitting each set if it is
   * empty
   */
  implicit val show: Show[RootShapes] =
    Show {
      case RootShapes(reals, imags) ⇒
        (
          if (reals.nonEmpty)
            show"$reals"
          else
            ""
        ) + (
          if (imags.nonEmpty)
            show"|$imags"
          else
            ""
        )
    }

  implicit val ordering: Ordering[RootShapes] = RootShapesOrdering.ordering
}

/* Quarantine this over here to not cross implicit-wires 😂 */
private object RootShapesOrdering {
  import hammerlab.ordering.generic, generic._
  import RootShape.descendingArity
  val ordering: Ordering[RootShapes] = generic[RootShapes]
}
