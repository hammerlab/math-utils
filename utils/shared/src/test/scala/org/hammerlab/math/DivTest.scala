package org.hammerlab.math

import hammerlab.Suite
import org.scalactic.TypeCheckedTripleEquals
import hammerlab.math.utils._

class DivTest
  extends Suite
    with TypeCheckedTripleEquals {

  test("ints") {
    ==(div( 0, 20), 0)
    ==(div(10, 20), 1)
    ==(div(19, 20), 1)
    ==(div(20, 20), 1)
    ==(div(21, 20), 2)
  }

  test("longs") {
    ==(div( 0L, 20L), 0L)
    ==(div(10L, 20L), 1L)
    ==(div(19L, 20L), 1L)
    ==(div(20L, 20L), 1L)
    ==(div(21L, 20L), 2L)

    ==(div(1L << 40, 1  <<  4), 1L << 36)
    ==(div(1L << 40, 1L << 36), 1L <<  4)
  }

  test("ceil") {
    for {
      n ← 1 to 10
    } withClue(s"$n: ") {
      ==(       n  /↑ 1, n)
      ==(       0  /↑ n, 0)
      ==(       1  /↑ n, 1)
      ==((  n - 1) /↑ n, if (n == 1) 0 else 1)
      ==(       n  /↑ n, 1)
      ==((  n + 1) /↑ n, 2)
      ==((2*n - 1) /↑ n, if (n == 1) 1 else 2)
      ==((2*n    ) /↑ n, 2)
      ==((2*n + 1) /↑ n, 3)
    }
  }
}
