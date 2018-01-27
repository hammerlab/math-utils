package cubic

import hammerlab.math.syntax._
import hammerlab.math.syntax.Arithmetic._

object Cubic {
  /**
   * Solve a cubic equation, ax³ + bx² + cx + d = 0, for a type [[D]] that conforms to various numeric type-classes
   *
   * @param a cubic term
   * @param b quadratic term
   * @param c linear term
   * @param d constant term
   * @param ε fuzzy-comparison tolerance, for calling [[Root.Double double]]- and [[Root.Triple triple]]-roots
   * @tparam D type of parameters and returned [[Root roots]]
   * @return [[Root Roots]] in increasing order
   */
  def apply[
      D: Math          // sqrt, ^, cos, acos
       : Arithmetic.I  // arithmetic between [[D]] instances
       : Arithmetic.D  // division by 2, 3
       : Doubleish     // lt/gt/eq comparisons to ints/doubles
  ](
      a: D,
      b: D,
      c: D,
      d: D
  )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
  ):
      Seq[Root[D]] =
    monic(
      b / a,
      c / a,
      d / a
    )

  def monic[
      D: Math          // sqrt, ^, cos, acos
       : Arithmetic.I  // arithmetic between [[D]] instances
       : Arithmetic.D  // division by 2, 3
       : Doubleish     // lt/gt/eq comparisons to ints/doubles
  ](
      b: D,
      c: D,
      d: D
   )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
   ):
  Seq[Root[D]] = {

    val b3 = b / 3
    val b32 = b3 * b3

    DepressedCubic[D](
      p = c - (3*b32: D),
      q = ((2 * b32):D) * b3 - b3*c + d
    )
    .map {
      _ - b3
    }
  }
}
