package org.hammerlab.math

import hammerlab.math.utils._
import org.hammerlab.Suite

class BinomialTest
  extends Suite {
  test("simple") {
    (0 to 10).foreach(    binomial(_, 0) should be(1))
    (1 to 10).foreach(n ⇒ binomial(n, 1) should be(n))
    (1 to 10).foreach(n ⇒ binomial(n, 2) should be(n * (n-1) / 2))
    (1 to 10).foreach(n ⇒ binomial(n, 3) should be(n * (n-1) * (n-2) / 6))
  }
}
