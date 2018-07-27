package org.hammerlab.math.format

import cats.syntax.show._
import hammerlab.math.exponent._

class ExponentTest
  extends hammerlab.Suite {
  test("format") {
    ===(1.show,  "")
    ===(2.show, "²")
    ===(3.show, "³")
    ===(4.show, "⁴")
    ===(5.show, "⁵")
    ===(6.show, "⁶")
    ===(7.show, "⁷")
    ===(8.show, "⁸")
    ===(9.show, "⁹")
  }
}
