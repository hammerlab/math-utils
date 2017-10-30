package hammerlab

import cats.implicits.catsKernelStdMonoidForString
import hammerlab.monoid._
import org.hammerlab.test.Suite

case class A(n: Int, s: String)

class MonoidTest
  extends Suite {
  test("case-class") {
    A(111, "aaa") |+| A(222, "bbb") should be(A(333, "aaabbb"))
  }
}

