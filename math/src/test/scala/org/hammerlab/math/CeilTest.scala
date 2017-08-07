package org.hammerlab.math

import org.hammerlab.test.Suite
import org.scalactic.TypeCheckedTripleEquals

class CeilTest
  extends Suite
    with TypeCheckedTripleEquals {

  test("ints") {
    ceil( 0, 20) should ===(0)
    ceil(10, 20) should ===(1)
    ceil(19, 20) should ===(1)
    ceil(20, 20) should ===(1)
    ceil(21, 20) should ===(2)
  }

  test("longs") {
    ceil( 0L, 20L) should ===(0)
    ceil(10L, 20L) should ===(1L)
    ceil(19L, 20L) should ===(1L)
    ceil(20L, 20L) should ===(1L)
    ceil(21L, 20L) should ===(2L)

    ceil(1L << 40, 1 << 4) should be(1L << 36)
    ceil(1L << 40, 1L << 36) should be(1 << 4)
  }
}
