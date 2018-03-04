package org.hammerlab.math

import hammerlab.math.utils._
import org.hammerlab.Suite

class InterpolateTest
  extends Suite {
  test("doubles") {
    interpolate(10, 20, 0.0) should be(10)
    interpolate(10, 20, 0.05) should be(10.5)
    interpolate(10, 20, 0.1) should be(11)
    interpolate(10, 20, 0.2) should be(12)
    interpolate(10, 20, 0.3) should be(13)
    interpolate(10, 20, 0.4) should be(14)
    interpolate(10, 20, 0.5) should be(15)
    interpolate(10, 20, 0.6) should be(16)
    interpolate(10, 20, 0.7) should be(17)
    interpolate(10, 20, 0.8) should be(18)
    interpolate(10, 20, 0.9) should be(19)
    interpolate(10, 20, 1.0) should be(20)
  }
}
