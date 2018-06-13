package hammerlab

import cats.implicits.catsKernelStdMonoidForString
import hammerlab.monoid._

case class A(n: Int, s: String)

class MonoidTest
  extends Suite {
  test("case-class") {
    val a1 = A(111, "aaa")
    val a2 = A(222, "bbb")
    val a3 = A(333, "aaabbb")

    ==(Seq(a1, a2).foldLeft(zero[A])(_ |+| _), a3)
  }
}

