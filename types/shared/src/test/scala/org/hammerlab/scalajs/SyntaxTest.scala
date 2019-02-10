package org.hammerlab.scalajs

class SyntaxTest
  extends hammerlab.Suite {
  test("imprecision") {
    import hammerlab.scalajs._
    // cf. https://github.com/scala-js/scala-js/issues/3202

    val actual = "%.11f".format(66380.788125)
    ===(
      actual,
      "66380.78812500000" js_? "66380.78812500001"
    )
    ===(
      actual,
      "66380.78812500001" jvm_? "66380.78812500000"
    )
  }
}
