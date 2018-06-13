package org.hammerlab.math.polynomial

import org.hammerlab.Suite
import spire.math.Complex
import ImaginaryRootPair.pairs
import spire.implicits._

class ImaginaryRootPairTest
  extends Suite {
  test("pairs") {
    ==(
      pairs(
        List(
          Complex(-6.000000011152852, 3.5025900029437364E-4),
          Complex(5.000000011152852 , 3.5025900029437364E-4),
          Complex(-6.000000011152852, -3.5025900029437364E-4),
          Complex(5.000000011152852, -3.5025900029437364E-4)
        )
      ),
      List(
        ImaginaryRootPair(-6.000000011152852, 3.5025900029437364E-4),
        ImaginaryRootPair( 5.000000011152852, 3.5025900029437364E-4)
      )
    )
  }
}
