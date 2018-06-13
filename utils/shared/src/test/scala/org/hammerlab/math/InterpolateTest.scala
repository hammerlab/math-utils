package org.hammerlab.math

import hammerlab.math.utils._
import org.hammerlab.Suite

class InterpolateTest
  extends Suite {
  test("doubles") {
    ==(interpolate(10, 20, 0.0 ), 10)
    ==(interpolate(10, 20, 0.05), 10.5)
    ==(interpolate(10, 20, 0.1 ), 11)
    ==(interpolate(10, 20, 0.2 ), 12)
    ==(interpolate(10, 20, 0.3 ), 13)
    ==(interpolate(10, 20, 0.4 ), 14)
    ==(interpolate(10, 20, 0.5 ), 15)
    ==(interpolate(10, 20, 0.6 ), 16)
    ==(interpolate(10, 20, 0.7 ), 17)
    ==(interpolate(10, 20, 0.8 ), 18)
    ==(interpolate(10, 20, 0.9 ), 19)
    ==(interpolate(10, 20, 1.0 ), 20)
  }
}
