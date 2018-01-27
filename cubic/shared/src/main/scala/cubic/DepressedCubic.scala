package cubic

import hammerlab.math.syntax._
import hammerlab.math.syntax.Arithmetic._
import hammerlab.math.syntax.FuzzyCmp._
import hammerlab.math.syntax.Math._

import scala.math.Pi

object DepressedCubic {
  /**
   * Solve a "depressed cubic" (quadratic term of 0; x³ + px + q = 0) for a type [[D]] that conforms to various numeric
   * type-classes
   *
   * @param p linear term
   * @param q constant term
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
      p: D,
      q: D
  )(
      implicit ε: Tolerance  // fuzzy lt/gt/eq comparisons
  ):
      Seq[Root[D]] = {

    import Root._

    if (p === 0 && q === 0)
      /**
       * Only possible triple-rooted depressed-cubic: x³=0
       *
       * Return the average of [[p]] and [[q]] to preserve flow of any gradient information, and have an instance of
       * [[D]], instead of just returning literal [[0]].
       */
      Seq(
        Triple(
          (p + q) / 2
        )
      )
    else {
      val p3 = -p/3
      val p33 = p3 ^ 3
      val q2 = -q/2
      val q22 = q2 ^ 2

      if (q22 <= p33) {
        val cos = q2 / p33.sqrt
        val sqp3 = p3.sqrt
        val sqp32: D = 2 * sqp3
        if (cos === 1)
          Seq(
            Double(-sqp3),
            Single(sqp32)
          )
        else if (cos === -1)
          Seq(
            Single(-sqp32),
            Double(sqp3)
          )
        else {
          val t0 = cos.acos / 3
          val t1 = t0 - pi23
          val t2 = t1 - pi23
          def root(t: D): D = sqp32 * t.cos
          Seq(
            Single(root(t2)),
            Single(root(t1)),
            Single(root(t0))
          )
        }
      } else {
        val sqr = (q22 - p33).sqrt
        Seq(
          Single(
            (q2 + sqr).cbrt +
            (q2 - sqr).cbrt
          )
        )
      }
    }
  }

  val pi23 = 2 * Pi / 3
}
