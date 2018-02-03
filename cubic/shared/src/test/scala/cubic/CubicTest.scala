package cubic

import org.hammerlab.math.polynomial.PolySolverTest
import org.hammerlab.math.syntax.Arithmetic._
import org.hammerlab.math.syntax.Doubleish
import org.hammerlab.math.syntax.Math._
import spire.algebra.{ Ring, Semiring }
import spire.math.Complex

import Seq.fill

class CubicTest
  extends PolySolverTest(3) {

  ε = 1e-8

  type R[T] = Root[T]
  type Real[T] = R[T]

  override def root[T](value: T, degree: Int) = Seq(Root(value, degree))

  val M: Int = 5


  test("roots sweep") {
    for {
      t @ TestCase(reals, _, _, Seq(a, b, c, d)) ← rootSweep
    } withClue(t.toString) {
      // Test that the solver returns the correct roots, given the coefficients
      ===(
        Cubic[Dbl](a, b, c, d),
        reals.map(Root.map(_.toDouble))
      )
    }
  }

  test("random roots") {
    import cubic.Root.doubleish
    for {
      t @ TestCase(reals, _, _, Seq(a, b, c, d)) <- randomCases(doubleish(Doubleish.id))
    } withClue(t.toString) {
      ===(
        Cubic[Dbl](a, b, c, d).toList,
        reals
      )
    }
  }

  import Root._

  test("high error") {
    ===(
      Cubic(
        -0.588472942148292,
        -3.064319020507687,
        -5.317625411104604,
        -3.0752531224202686
      ),
      Seq(
        Single(-1.7890641803834801),
        Double(-1.7090872107407873)
      )
    )
  }

  /**
   * Found during fuzz-testing: actually need more than "double"-precision to get this one right.
   *
   * It is of the form (x - r)²(x - r - ε) = 0, where r == 0.9680353382997225 and ε == 8.626853443584048E-4
   *
   * `q` in the depressed cubic is -2ε³/27 == -4.755799007395558E-11, but computing it from the coefficients yields
   * 2b³/27 - bc/3 + d == -4.7557957572053056E-11, which differs from the true value by ≈6.8/10⁷; which ends up being
   * too high for a [[ε]] of 1e-7.
   *
   * On the other hand, a [[ε]] of 1e-6 is too coarse, and the double and single roots (separated by ε) are collapsed
   * into a rounded-off triple-root.
   */
  ignore("false triple") {
    ===(
      Cubic(
        0.07512278662356181,
        -0.2182293438165201,
        0.21131645245111425,
        -0.06820750799716728
      ),
      Seq(
        Double(0.9680353382997225),
        Single(0.968898023644081)
      )
    )
  }

  override def toComplex[T: Ring](r: Root[T]): Seq[Complex[T]] = fill(r.degree)(Complex(r.value))
}
