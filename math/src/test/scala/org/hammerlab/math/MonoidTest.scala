package org.hammerlab.math

import org.hammerlab.test.Suite
import MonoidSyntax._
import Monoid.zero

class MonoidTest
  extends Suite {
  test("case class") {
    case class Foo(a: Int, b: String, c: Long)

    val foo1 = Foo(111, "abc", 123)
    val foo2 = Foo(222, "def", 456)

    foo1 |+| foo2 should be(Foo(333, "abcdef", 579))
    foo1 |+| zero[Foo] should be(foo1)
    foo2 |+| zero[Foo] should be(foo2)
  }
}
