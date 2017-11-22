package org.hammerlab.math

import cats.Show
import cats.syntax.show._
import org.hammerlab.test.Suite
import spire.math._

class FormatTest
  extends Suite {

  def check[I : Integral : Show](i: I, expected: String): Unit = {
    i.show should be(expected)
  }

  test("2-digit ints") {
    import scientific.digits2

    (-20 to 20).foreach(n ⇒ check(n, n.toString))

    check(   -100,   "-100")

    check(     99,     "99")
    check(    100,    "100")
    check(    101,    "101")

    check(    999,    "999")
    check(   1000,   "1000")
    check(   1001,   "1001")

    check(   9999,   "9999")
    check(  10000,  "10000")
    check(  10001,  "10001")

    check(  99999,  "99999")
    check( 100000,  "1.0e5")
    check( 100001,  "1.0e5")

    check( 104999,  "1.0e5")
    check( 105000,  "1.1e5")
    check( 105001,  "1.1e5")

    check( 144999,  "1.4e5")
    check( 145000,  "1.5e5")
    check( 145001,  "1.5e5")

    check( 149999,  "1.5e5")
    check( 150000,  "1.5e5")
    check( 150001,  "1.5e5")

    check( 199999,  "2.0e5")
    check( 200000,  "2.0e5")
    check( 200001,  "2.0e5")

    check( 449999,  "4.5e5")
    check( 450000,  "4.5e5")
    check( 450001,  "4.5e5")

    check(-494999, "-4.9e5")
    check(-495000, "-5.0e5")
    check(-495001, "-5.0e5")

    check( 494999, "4.9e5")
    check( 495000, "5.0e5")
    check( 495001, "5.0e5")

    check( 994999, "9.9e5")
    check( 995000, "1.0e6")
    check( 995001, "1.0e6")

    check( 999999, "1.0e6")
    check(1000000, "1.0e6")
    check(1000001, "1.0e6")

    check(1049999, "1.0e6")
    check(1050000, "1.1e6")
  }

  test("3-digit ints") {
    import scientific.digits3

    (-20 to 20).foreach(n ⇒ check(n, n.toString))

    check(    -100,   "-100")

    check(      99,     "99")
    check(     100,    "100")
    check(     101,    "101")

    check(     999,    "999")
    check(    1000,   "1000")
    check(    1001,   "1001")

    check(    9999,   "9999")
    check(   10000,  "10000")
    check(   10001,  "10001")

    check(   99999,  "99999")
    check(  100000, "100000")
    check(  100001, "100001")

    check(  999999, "999999")
    check( 1000000, "1.00e6")
    check( 1000001, "1.00e6")

    check( 1004999, "1.00e6")
    check( 1005000, "1.01e6")
    check( 1005001, "1.01e6")

    check( 1044999, "1.04e6")
    check( 1045000, "1.05e6")
    check( 1045001, "1.05e6")

    check( 1049999, "1.05e6")
    check( 1050000, "1.05e6")
    check( 1050001, "1.05e6")

    check( 1944999, "1.94e6")
    check( 1945000, "1.95e6")
    check( 1945001, "1.95e6")

    check( 1994999, "1.99e6")
    check( 1995000, "2.00e6")
    check( 1995001, "2.00e6")

    check( 1999999, "2.00e6")
    check( 2000000, "2.00e6")
    check( 2000001, "2.00e6")

    check( 9994999, "9.99e6")
    check(10000000, "1.00e7")
    check(10000001, "1.00e7")

    check(10049999, "1.00e7")
    check(10050000, "1.01e7")
    check(10050001, "1.01e7")
  }
}
