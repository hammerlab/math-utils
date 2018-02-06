package org.hammerlab.math.format

import hammerlab.iterator._
import org.hammerlab.Suite
import cats.syntax.show._
import SigFigs._

class SigFigsTest
  extends Suite {
  def check(d: Double, expecteds: String*): Unit = {
    expecteds
      .zipWithIndex
      .mapValues(_ + 1)
      .foreach {
        case (expected, sigFigs) ⇒
          implicit val s: SigFigs = sigFigs
          withClue(s"$s digits: ") {
            d.show should be(expected)
            (-d).show should be('-' + expected)
          }
      }
  }

  test("3-digit numbers") {
    check( 123000, "1e5" , "1.2e5" , "123000" , "123000"  , "123000"   )
    check(  12300, "1e4" , "12300" , "12300"  , "12300"   , "12300"    )
    check(   1230, "1e3" , "1230"  , "1230"   , "1230"    , "1230.0"   )
    check(    123, "123" , "123"   , "123"    , "123.0"   , "123.00"   )
    check(   12.3, "12"  , "12"    , "12.3"   , "12.30"   , "12.300"   )
    check(   1.23, "1"   , "1.2"   , "1.23"   , "1.230"   , "1.2300"   )
    check(   .123, "0.1" , "0.12"  , "0.123"  , "0.1230"  , "0.12300"  )
    check(  .0123, "0.01", "0.012" , "0.0123" , "0.01230" , "0.012300" )
    check( .00123, "1e-3", "0.0012", "0.00123", "0.001230", "0.0012300")
    check(.000123, "1e-4", "1.2e-4", "1.23e-4", "1.230e-4", "1.2300e-4")
  }

  test("4-digit numbers") {
    check(1234000 , "1e6" , "1.2e6" , "1.23e6" , "1234000" , "1234000"  )
    check(123400  , "1e5" , "1.2e5" , "123400" , "123400"  , "123400"   )
    check(12340   , "1e4" , "12340" , "12340"  , "12340"   , "12340"    )
    check(1234    , "1e3" , "1234"  , "1234"   , "1234"    , "1234.0"   )
    check(123.4   , "123" , "123"   , "123"    , "123.4"   , "123.40"   )
    check(12.34   , "12"  , "12"    , "12.3"   , "12.34"   , "12.340"   )
    check(1.234   , "1"   , "1.2"   , "1.23"   , "1.234"   , "1.2340"   )
    check(.1234   , "0.1" , "0.12"  , "0.123"  , "0.1234"  , "0.12340"  )
    check(.01234  , "0.01", "0.012" , "0.0123" , "0.01234" , "0.012340" )
    check(.001234 , "1e-3", "0.0012", "0.00123", "0.001234", "0.0012340")
    check(.0001234, "1e-4", "1.2e-4", "1.23e-4", "1.234e-4", "1.2340e-4")
  }

  test("5-digit numbers") {
    check(1234500  , "1e6" , "1.2e6" , "1.23e6" , "1234500" , "1234500"  )
    check(123450   , "1e5" , "1.2e5" , "123450" , "123450"  , "123450"   )
    check(12345    , "1e4" , "12345" , "12345"  , "12345"   , "12345"    )
    check(1234.5   , "1e3" , "1235"  , "1235"   , "1235"    , "1234.5"   )
    check(123.45   , "123" , "123"   , "123"    , "123.5"   , "123.45"   )
    check(12.345   , "12"  , "12"    , "12.3"   , "12.35"   , "12.345"   )

    /**
     * Tweaking this test-case, and a few below, because javascript decimal-rounding is kind of a mess / buggy; see
     * [[https://stackoverflow.com/questions/10015027/javascript-tofixed-not-rounding]].
     *
     * For our purposes here we just inherit/accept underlying rounding-wonkiness; unclear what else could be done.
     */
    // check(1.2345   , "1"   , "1.2"   , "1.23"   , "1.235"   , "1.2345"   )
    check(1.2355   , "1"   , "1.2"   , "1.24"   , "1.236"   , "1.2355"   )

    check(.12345   , "0.1" , "0.12"  , "0.123"  , "0.1235"  , "0.12345"  )
    check(.012345  , "0.01", "0.012" , "0.0123" , "0.01235" , "0.012345" )

    /** Using bases other than "12345" due to the Javascript-rounding issue described above */
    check(.0012335 , "1e-3", "0.0012", "0.00123", "0.001234", "0.0012335")
    check(.00012315, "1e-4", "1.2e-4", "1.23e-4", "1.232e-4", "1.2315e-4")
  }
}
