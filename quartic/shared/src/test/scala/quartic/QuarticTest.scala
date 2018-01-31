package quartic

import cats.syntax.show._
import hammerlab.iterator._
import org.hammerlab.Suite
import org.hammerlab.math.format.showSuperscript
import spire.algebra.{ Ring, Semiring }
import spire.implicits._
import spire.math.Complex

import scala.Array.fill

class QuarticTest
  extends Suite {

  type Dbl = scala.Double

  val N = 4  // degree of a quartic polynomial

  // Maximum root value (positive or negative) to sweep over while checking [[Quartic]]-solver
  val M = 4

  type R = Root[Int]

  type Coeffs

//  /**
//   * Iterator over all sets of `num` integer-roots in the range [-M,M] (including double/triple/quadruple roots)
//   */
//  def realRootsIter(num: Int) =
//    (-M to M)
//      .unorderedSubsetsWithReplacement(num)
//      .map {
//        _.map {
//          case (root, arity) ⇒
//            Root(root, arity)
//        }
//      }
//
//  val allImaginaryRootPairs =
//    for {
//      a ← -M to M
//      b ←  1 to M
//    } yield
//      ImaginaryRootPair(a, b)
//
  /**
   * A pair of conjugate imaginary numbers, [[a]]±[[b]]i, both of which are roots of a quartic polynomial
   *
   * We're testing polynomials with real coefficients, so imaginary roots must come in conjugate-pairs
   *
   * [[b]] must be greater than zero; real roots are not modeled with this data structure
   */
//  case class ImaginaryRootPair[T](a: T, b: T) {
//    override def toString: String =
//      (if (a == 0) "" else s"$a") +
//      "±" +
//      (if (b == 1) "" else s"$b") +
//      "i"
//  }
//
//  implicit class ImaginaryRootPairOps[T](r: ImaginaryRootPair[T]) {
//    def complex(implicit e: Ring[T]): Seq[Complex[T]] =
//      Seq(
//        Complex(r.a, -r.b),
//        Complex(r.a,  r.b)
//      )
//  }
//
//  implicit class RootOps[T](r: Root[T]) {
//    def complex(implicit e: Semiring[T]): Array[Complex[T]] =
//      fill(
//        r.degree
//      )(
//        Complex(r.value)
//      )
//  }
//
//  import cats.Show
//  import Show.show
//
//  /**
//   * Pretty-print an imaginary-conjugate root-pair, possibly repeated as a multiple root (denoted by a superscript, as
//   * with [[Root]])
//   */
//  implicit def showImagRoot[T]: Show[(ImaginaryRootPair[T], Int)] =
//    show {
//      case (pair, 1) ⇒ pair.toString
//      case (pair, degree) ⇒ s"($pair)${showSuperscript.show(degree)}"
//    }

//  /**
//   * Iterate over the available imaginary-root conjugate-pairs (with integral real and imaginary parts in [-M, M], per
//   * [[allImaginaryRootPairs]]), emitting all possible multi-sets of size `num`, and allowing repeats of individual
//   * pairs (representing multiple roots)
//   */
//  def imaginaryRootPairs(num: Int) =
//    allImaginaryRootPairs
//      .unorderedSubsetsWithReplacement(num)



  /**
   * Generate all possible sets of 4 roots of a quartic equation (real or imaginary), derive coefficients, solve
   * quartic, verify that correct real roots are returned
   */
  test("roots sweep") {
    for {
      numImaginaryRootPairs ← 0 to N/2
      numImaginaryRoots = 2 * numImaginaryRootPairs
      numRealRoots = N - numImaginaryRoots
      realRoots ← realRootsIter(numRealRoots)
      imagRoots ← imaginaryRootPairs(numImaginaryRootPairs)
    } {
      /**
       * Expand multiple roots, convert to [[Complex]] for arithmetic with imaginary roots
       */
      val reals = realRoots.flatMap(_.complex)

      /**
       * Expand multiple roots, convert to [[Complex]] for arithmetic
       */
      val imags =
        imagRoots
          .flatMap {
            case (ab, d) ⇒
              fill(d)(ab)
          }
          .flatMap(_.complex)

      /** All roots, as [[Complex]] */
      val roots = reals ++ imags

      val (b, c, d, e) = coeffs(roots)

      // Test that the solver returns the correct roots, given the coefficients
       println(s"roots: ${(realRoots ++ imagRoots.map(_.show)).mkString(" ")}\t coeffs: $b $c $d $e")
      withClue(s"roots: ${(realRoots ++ imagRoots.map(_.show)).mkString(" ")}\t coeffs: $b $c $d $e: ") {
        ===(
          Quartic.monic[Dbl](b, c, d, e).toList,
          realRoots.map(Root.map(_.toDouble))
        )
      }
    }
  }

  import scala.util.Random._
  def rnd(): Dbl = {
    nextGaussian()
//    val y = nextDouble()
    // inverse sigmoid: (0,1) ⟶ (-∞,∞)
//    log(y / (1 - y))
  }

  def coeffs[T: Ring](roots: Seq[Complex[T]]): (T, T, T, T) = {
    /** Infer the coefficients of the quartic equation from the roots */
    val Seq(b, c, d, e) =
      (
        for {
          coeff ← 1 to N
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

    (b, c, d, e)
  }

  test("quad root") {
    val value = 0.22606201283216426
    val root = Root(value, 4)
    val (b, c, d, e) = coeffs(root.complex)
    ===(
      Quartic.monic(b, c, d, e),
      Seq(root)
    )
  }

  test("biquad") {
    val roots = Seq(Root(-1.4056993567411216, 2), Root(0.4866766263606559, 2))
    val (b, c, d, e) = coeffs(roots.flatMap(_.complex))
    ===(
      Quartic.monic(b, c, d, e),
      roots
    )
  }

  test("random roots") {
    setSeed(123)
    for {
      numImagPairs ← 0 to N/2
      numImagRoots = 2 * numImagPairs
      imagPairArities ← numImagPairs.unorderedPartitions

      imagRoots = imagPairArities.map(ImaginaryRootPair(rnd(), rnd()) → _)
      imagRootPairs = imagRoots.flatMap { case (pair, arity) ⇒ fill(arity)(pair) }
      imags = imagRootPairs.flatMap(_.complex)

      numRealRoots = N - numImagRoots
      realRootArities ← numRealRoots.unorderedPartitions

      // do 10 reps with each possible arity-distribution of [imaginary X real] roots
      iteration ← 1 to 10

      realRoots = realRootArities.map(Root(rnd(), _)).sortBy(_.value)
      reals = realRoots.flatMap(_.complex)
      roots = reals ++ imags
      (b, c, d, e) = coeffs(roots)
    } {
       println(s"iteration $iteration:\troots: ${(realRoots ++ imagRoots.map(_.show)).mkString(" ")}\t coeffs: $b $c $d $e")
      withClue(s"iteration $iteration:\troots: ${(realRoots ++ imagRoots.map(_.show)).mkString(" ")}\t coeffs: $b $c $d $e: ") {
        ===(
          Quartic.monic[Dbl](b, c, d, e).toList,
          realRoots
        )
      }
    }
  }
}
