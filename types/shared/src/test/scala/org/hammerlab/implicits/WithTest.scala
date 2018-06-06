package org.hammerlab.implicits

import cats.Show

trait ShowInt {
  var numDerivations = 0

  implicit def showInt: Show[Int] = {
    numDerivations += 1
    new Show[Int] {
      def show(n: Int) = s"$n$n"
    }
  }
}

class WithTest
  extends hammerlab.Suite
    with ShowInt {

  import cats.syntax.show._

  test("usage") {

    wth[cats.Show[Int]] → {
      /** name has to shadow [[showInt]] above */
      implicit showInt: Show[Int] ⇒

        toShow(123).show should be("123123")
        456.show should be("456456")
    }

    numDerivations should be(1)

    `with`[cats.Show[Int]] → {
      implicit showInt ⇒
        123.show should be("123123")
        456.show should be("456456")
    }

    numDerivations should be(2)
  }
}
