package org.hammerlab

import hammerlab.reflect._

class ReflectOpsTest
  extends hammerlab.Suite {
  val foo = Foo(123)

  test("call private methods") {
    ==(
      foo.get_!('n2),
      246
    )
    ==(
      foo.get_!("n2"),
      246
    )

    // get private method
    ==(
      foo.get_!('_mult, 10),
      1230
    )

    ==(
      foo.get_!('_concat, "aaa"),
      "123aaa"
    )
  }

  test("call public method") {
    // get public method
    ==(
      foo.get_!('mult, 10),
      1230
    )
  }

  test("get/update private val") {
    ==(
      foo.get_!('str),
      "123"
    )

    foo.set_!('str, "abc")

    ==(
      foo.get_!('str),
      "abc"
    )
  }

  test("call static methods") {
    ==(
      Foo.get_!('mult, 2, "abc"),
      "abcabc"
    )
    ==(
      Foo.get_!('_mult, 2, "abc"),
      "abcabc"
    )
  }

  test("call java static method") {
    import org.hammerlab.reflect.test.Bar
    ==(
      get_![Bar, Int]('n),
      2
    )

    ==(
      get_![Bar, Int]('nn),
      4
    )

    set_![Bar]('n, 10)

    ==(
      get_![Bar, Int]('n),
      10
    )

    ==(
      get_![Bar, Int]('nn),
      20
    )
  }
}

case class Foo(n: Int) {
  private def n2: Int = n * 2
  def n4 = n2 * n2

  private def _mult(r: Int): Int = n * r
  def mult(r: Int) = _mult(r)

  private def _concat(s: String): String = str + s

  private val str = n.toString
  def getStr = str
}
object Foo {
  private def _mult(n: Int, s: String): String = s * n
  def mult(n: Int, s: String): String = _mult(n, s)
}
