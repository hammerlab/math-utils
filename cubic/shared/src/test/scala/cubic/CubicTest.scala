package cubic

import hammerlab.math.syntax.Arithmetic._
import hammerlab.math.syntax.Math._
import cubic.Root._
import org.hammerlab.Suite

import scala.scalajs.js.annotation.JSExport

@JSExport
class CubicTest
  extends Suite {

  type Dbl = scala.Double

  ε = 1e-10

  /**
   * Values of the "a" coefficient (of the x³ term) to test for each case; "b", "c", and "d" coefficients get multiplied
   * by this value.
   */
  val scales = Seq(1, 2, 3)

  def check(dbl: Dbl, other: Dbl): Unit = check(Double(dbl), Single(other))

  def check(r1: Dbl, r2: Dbl, r3: Dbl): Unit = check(Single(r1), Single(r2), Single(r3))

  def check(roots: Root[Dbl]*): Unit = {
    val Seq(r1, r2, r3) = roots.flatMap(r ⇒ Array.fill(r.degree)(r.value))
    val b = -r1 - r2 - r3
    val c = r1*r2 + r1*r3 + r2*r3
    val d = -r1 * r2 * r3
    coeffs(b, c, d)(roots: _*)
  }

  def coeffs(b: Dbl, c: Dbl, d: Dbl)(roots: Root[Dbl]*) = {
    scales foreach {
      a ⇒
        ===(
          Cubic[Dbl](a, b * a, c * a, d * a),
          roots.sortBy(_.value)
        )
    }
  }

  test("distinct roots") {

    def checkAll(r1: Dbl,
                 r2: Dbl,
                 r3: Dbl) = {
      check( r1,  r2,  r3)
      check( r1,  r2, -r3)
      check( r1, -r2,  r3)
      check( r1, -r2, -r3)
      check(-r1,  r2,  r3)
      check(-r1,  r2, -r3)
      check(-r1, -r2,  r3)
      check(-r1, -r2, -r3)
    }

    checkAll(  1,  2,  3)
    checkAll(0.1,  1, 10)
  }

  test("double root") {

    def checkSigns(dbl: Dbl, other: Dbl) = {
      check(Double( dbl), Single( other))
      check(Double(-dbl), Single( other))
      check(Double( dbl), Single(-other))
      check(Double(-dbl), Single(-other))
    }

    checkSigns(1,  2)
    checkSigns(1,  3)
    checkSigns(1, 10)

    checkSigns( 2, 1)
    checkSigns( 3, 1)
    checkSigns(10, 1)
  }

  test("triple root") {
    check(Triple(  1))
    check(Triple( -1))
    check(Triple( 10))
    check(Triple(-10))
  }

  test("single root") {
    def chk(root: Dbl, a: Dbl, c: Dbl) = {
      val c2 = -(root + 2 * a)
      val c1 = 2 * root * a + a*a + c*c
      val c0 = -root * (a*a + c*c)
      withClue(s"(x-$root)(x-$a-${c}i)(x-$a+${c}i") {
        coeffs(c2, c1, c0)(Single(root))
      }
    }

    def checkSigns(root: Dbl, a: Dbl, c: Dbl) = {
      chk( root,  a,  c)
      chk( root,  a, -c)
      chk( root, -a,  c)
      chk( root, -a, -c)
      chk(-root,  a,  c)
      chk(-root,  a, -c)
      chk(-root, -a,  c)
      chk(-root, -a, -c)
    }

    def checkPermutations(a: Dbl, b: Dbl, c: Dbl) = {
      checkSigns(a, b, c)
      checkSigns(a, c, b)
      checkSigns(b, a, c)
      checkSigns(b, c, a)
      checkSigns(c, a, b)
      checkSigns(c, b, a)
    }

    checkSigns(0, 1, 1)
    checkSigns(1, 0, 1)
    checkSigns(1, 1, 1)

    checkPermutations( 1, 2,  3)
    checkPermutations(.1, 1, 10)
  }
}
