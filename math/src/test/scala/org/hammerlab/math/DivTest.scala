package org.hammerlab.math

import org.hammerlab.test.Suite
import org.scalactic.TypeCheckedTripleEquals

class DivTest
  extends Suite
    with TypeCheckedTripleEquals {

  test("ints") {
    div( 0, 20) should ===(0)
    div(10, 20) should ===(1)
    div(19, 20) should ===(1)
    div(20, 20) should ===(1)
    div(21, 20) should ===(2)
  }

  test("longs") {
    div( 0L, 20L) should ===(0)
    div(10L, 20L) should ===(1L)
    div(19L, 20L) should ===(1L)
    div(20L, 20L) should ===(1L)
    div(21L, 20L) should ===(2L)

    div(1L << 40, 1 << 4) should be(1L << 36)
    div(1L << 40, 1L << 36) should be(1 << 4)
  }
}
