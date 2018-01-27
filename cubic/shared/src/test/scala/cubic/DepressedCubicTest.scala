package cubic

import cubic.Root.Single
import hammerlab.math.syntax.Arithmetic._
import hammerlab.math.syntax.Math._
import org.hammerlab.Suite

import scala.Array.fill
import scala.math.{ cbrt, sqrt }
import scala.scalajs.js.annotation.JSExport

@JSExport
class DepressedCubicTest
  extends Suite {

  ε = 1e-10

  type Dbl = Double

  implicit def intToRoot(n: Int): Root[Dbl] = Single(n)
  implicit def doubleToRoot(n: Dbl): Root[Dbl] = Single(n)

  def chk(p: Dbl, q: Dbl)(expected: Root[Dbl]*): Unit =
    ===(
      DepressedCubic(p, q),
      expected
    )

  import Root._

  test("p == 0") {
    chk(0,  0)(Triple(0))
    chk(0,  1)(-1)
    chk(0, -1)( 1)
    chk(0, -2)(cbrt(2))
    chk(0,  2)(-cbrt(2))
    chk(0, -8)( 2)
    chk(0,  8)(-2)
  }

  test("q == 0") {
    chk( 1, 0)( 0)
    chk( 2, 0)( 0)
    chk(-1, 0)(-1, 0, 1)
    chk(-2, 0)(-sqrt(2), 0, sqrt(2))
  }

  test("3 roots") {

    def check(roots: Root[Dbl]*) = {
      val Seq(r1, r2, r3) = roots.flatMap(r ⇒ fill(r.degree)(r.value))
      val p = r1*r2 + r1*r3 + r2*r3
      val q = -r1 * r2 * r3

      chk(
        p, q
      )(
        roots: _*
      )

      chk(
        p, -q
      )(
        roots.map(map[Dbl](-_)(_)).reverse: _*
      )
    }

    check(-2, -1, 3)
    check(-6,  Double(3))
    check(-3, -1, 4)
  }

  test("1 root") {
    /**
     * Test root-finding of a depressed cubic with roots { a+ci, a-ci, and -2a } (i.e. where p == c²-3a² and
     * q == 2a(c² + a²).
     */
    def check(a: Dbl, c: Dbl) = {
      chk(
        c*c - 3*a*a,
        2*a*(c*c + a*a)
      )(
        Single(-2 * a)
      )
    }

    def checkSigns(a: Dbl, c: Dbl) = {
      check( a,  c)
      check(-a,  c)
      check( a, -c)
      check(-a, -c)
    }

    for {
      a ← 1 to 3
      c ← 1 to 3
    } {
      checkSigns(a, c)
    }
  }
}
