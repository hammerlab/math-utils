package org.hammerlab.math

import hammerlab.math.utils._

class SafeIntTest
  extends hammerlab.Suite {
  test("safe") {
    ==( 123L.safeInt, Right( 123))
    ==(-123L.safeInt, Right(-123))
    ==(   0 .safeInt, Right(   0))

    ==(- 1L << 31      safeInt, Right( Integer.MIN_VALUE ))
    ==( (1L << 31) - 1 safeInt, Right( Integer.MAX_VALUE ))

    ==(  1L << 31      safeInt,  Left( CastException(  1L << 31     )))
    ==((-1L << 31) - 1 safeInt,  Left( CastException((-1L << 31) - 1)))

    ==(  1L << 33 safeInt "descriptive msg", Left(CastException(1L << 33, "descriptive msg")))
  }

  test("!") {
    ==( 123L.int_!,  123)
    ==(-123L.int_!, -123)
    ==(   0 .int_!,    0)

    ==(- 1L << 31      int_!, Integer.MIN_VALUE )
    ==( (1L << 31) - 1 int_!, Integer.MAX_VALUE )

    ==(intercept[CastException] {   1L << 31      int_! }, CastException(  1L << 31     ))
    ==(intercept[CastException] { (-1L << 31) - 1 int_! }, CastException((-1L << 31) - 1))

    ==(
      intercept[CastException] {
        (1L << 33) int_! "descriptive msg"
      },
      CastException(
        1L << 33,
        "descriptive msg"
      )
    )
  }
}
