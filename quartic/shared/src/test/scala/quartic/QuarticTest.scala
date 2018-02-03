package quartic

import hammerlab.iterator._
import org.hammerlab.math.polynomial.PolySolverTest
import spire.algebra.Ring
import spire.implicits._

class QuarticTest
  extends PolySolverTest(4) {

  // Maximum root value (positive or negative) to sweep over while checking [[Quartic]]-solver
  val M = 4

  test("roots sweep") {
    for {
      t @ TestCase(reals, _, _, Seq(a, b, c, d, e)) ← rootSweep
    } withClue(t.toString) {
      // Test that the solver returns the correct roots, given the coefficients
      ===(
        Quartic[Dbl](a, b, c, d, e),
        reals.map(Root.map(_.toDouble))
      )
    }
  }

  def check(t: TestCase[Dbl]): Unit = {
    val TestCase(reals, _, _, Seq(a, b, c, d, e)) = t
    withClue(t.toString + ":\n") {
      ===(
        Quartic(a, b, c, d, e).toList,
        reals
      )
    }
  }

  test("random roots") {
    for {
      t @ TestCase(reals, _, _, Seq(a, b, c, d, e)) ← randomCases
    } withClue(t.toString + ":\n") {
      ===(
        Quartic[Dbl](a, b, c, d, e).toList,
        reals
      )
    }
  }


  import Root._
  test("simple") {
    ===(
      Quartic[Dbl](-3, -27, -69, -9, 108),
      Seq(
        Single(-4),
        Double(-3),
        Single(1)
      )
      .map(Root.map(_.toDouble))
    )
  }

  test("hard random") {
    check(
      TestCase(
        Seq(
          Single(0.7056936492337609),
          Triple(0.711645855752505)
        ),
        Nil,
        -0.8856759931799498
      )
    )
  }

  test("quad") {
    check(
      TestCase(
        Seq(
          Quadruple(-1.4380493091409068)
        ),
        Nil,
        -0.9055902426020439
      )
    )
  }

  test("triple") {
    check(
      TestCase(
        Seq(
          Single(-2.6276823667286164),
          Triple(1.8813459976839522)
        ),
        Nil,
        -1.4820776342688322
      )
    )
  }

  test("doubles") {
    check(
      TestCase(
        Seq(
          Double(0.4281587441748684),
          Double(0.43363370776141763)
        ),
        Nil,
        0.47977682695867746
      )
    )
  }

  override val casePrintInterval: Int = 1

  override type R[T] = Root[T]

  //  test("quad root") {
//    val value = 0.22606201283216426
//    val root = Root(value, 4)
//    val (b, c, d, e) = coeffs(root.complex)
//    ===(
//      Quartic.monic(b, c, d, e),
//      Seq(root)
//    )
//  }
//
//  test("biquad") {
//    val roots = Seq(Root(-1.4056993567411216, 2), Root(0.4866766263606559, 2))
//    val (b, c, d, e) = coeffs(roots.flatMap(_.complex))
//    ===(
//      Quartic.monic(b, c, d, e),
//      roots
//    )
//  }
  override def root[T: Ring](value: T, degree: Int): Root[T] = Root(value, degree)
}
