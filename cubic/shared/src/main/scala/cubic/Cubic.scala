package cubic

import org.hammerlab.math.syntax._
import org.hammerlab.math.syntax.Arithmetic._
import FuzzyCmp._
import Math._
import Root.Triple

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

    import Doubleish.DoubleishOps
    import math.abs
    val M =
      Seq(
        abs(b.toDouble),
        abs(c.toDouble),
        abs(d.toDouble)
      )
      .max

    val b3 = b / 3
    val b32 = b3 * b3
    val b323 = b32 * 3
    val b322 = b32 * 2

    val p = c - b323
    val q = b3 * b322 - b3*c + d

    def eq(l: D, r: D): Boolean =
      M + abs(l.toDouble) === M + abs(r.toDouble)

    println(s"\tdep cub: b $b c $c d $d p $p q $q b323 $b323 b32*b3 ${b32*b3} c==b323 ${c === b323} ${eq(c, b323)} d===b32*b3 ${d === b32 * b3} ${eq(d, b32 * b3)}")

    /** zero-checks inside [[DepressedCubic]] are tricky with relative/geometric "fuzziness", so do them out here */
    if (eq(c, b323) && eq(d, b32 * b3)) {
      println(s"\ttriple: $b $c $d, $b3 $b323 ${b32 * b3}")
      Seq(
        Triple(-b3)
      )
    } else
      DepressedCubic(
        c - b323,
        b3 * b322 - b3*c + d
      )
      .map {
        _ - b3
      }
  }
}
