package org.hammerlab.math.polynomial

import hammerlab.show._
import hammerlab.iterator._
import spire.algebra.Ring
import spire.math.Complex
import spire.implicits._

import scala.Array.fill

case class TestCase[T]( reals: Seq[(Real[T], Int)],
                        imags: Seq[(ImaginaryRootPair[T], Int)],
                        roots: Seq[Complex[T]],
                       coeffs: Seq[T]) {
  def scale = coeffs.head
}

object TestCase {
  def apply[T: Ring](reals: Seq[(Real[T], Int)],
                     imags: Seq[(ImaginaryRootPair[T], Int)],
                     scale: T): TestCase[T] = {
    val roots =
      reals
        .flatMap { case (ab, d) ⇒ fill(d)(ab) }
        .map { case Real(r) => Complex(r) } ++
      imags
        .flatMap { case (ab, d) ⇒ fill(d)(ab) }
        .flatMap(_.complex)

    TestCase(
      reals,
      imags,
      roots,
      coeffs(roots, scale)
    )
  }

  def coeffs[T: Ring](roots: Seq[Complex[T]], scale: T): Seq[T] =
    /** Infer the coefficients of the quartic equation from the roots */
    scale ::
    (
      for {
        coeff ← 1 to roots.size
      } yield
        roots
          .unorderedSubsets(coeff)
          .map(_.reduce(_ * _))
          .reduce(_ + _) *
            (
              if (coeff % 2 == 0)
                Ring[T].fromInt(1)
              else
                Ring[T].fromInt(-1)
            )
    )
    .map(_.real)  // coefficients will always be real because all imaginary roots come in conjugate-pairs
    .map(_ * scale)
    .toList

  implicit def showTestCase[T: Show]: Show[TestCase[T]] =
    Show {
      case TestCase(reals, imags, _, coeffs) ⇒
        import Root.showRoot
        val roots: Seq[(Root[T], Int)] = reals ++ imags
        show"roots: ${roots.map(_.show).mkString(", ")}\t coeffs: ${coeffs.map(_.show).mkString(" ")}"
    }
}

