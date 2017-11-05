package hammerlab

import hammerlab.either._
import org.hammerlab.test.Suite

class EitherTest
  extends Suite {
  test("apply/unapply") {
    val l = L(4)
    val r = R("a")
    val b = Both(4, "a")

    val L(n) = l
    n should be(4)

    val R(a) = r
    a should be("a")

    val Both(m, s) = b
    m should be(4)
    s should be("a")
  }
}
