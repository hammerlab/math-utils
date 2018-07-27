package org.hammerlab.math.format

import hammerlab.math.exponent.cased

class ExponentCaseShorthand
  extends SigFigsTest {
  test("3-digits: lower- vs upper-case exponent markers") {
    check( 123000, "1E5", "1.2e5", "123000", "123000" , "123000"  )
    check(  12300, "1E4", "12300", "12300" , "12300"  , "12300"   )
    check(   1230, "1E3", "1230" , "1230"  , "1230"   , "1230.0"  )
    check(    123, "123", "123"  , "123"   , "123.0"  , "123.00"  )
    check(   12.3, "12" , "12"   , "12.3"  , "12.30"  , "12.300"  )
    check(   1.23, "1"  , "1.2"  , "1.23"  , "1.230"  , "1.2300"  )
    check(   .123, "0.1", "0.12" , "0.123" , "0.1230" , "0.12300" )
    check(  .0123, "1e2", "0.012", "0.0123", "0.01230", "0.012300")
    check( .00123, "1e3", "1.2e3", "1.23e3", "1.230e3", "1.2300e3")
    check(.000123, "1e4", "1.2e4", "1.23e4", "1.230e4", "1.2300e4")
  }
}
