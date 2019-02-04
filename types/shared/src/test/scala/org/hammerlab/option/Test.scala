package org.hammerlab.option

import hammerlab.option._

class Test
  extends hammerlab.Suite {
  test("syntax") {
    ==(true ? 4, Some(4))
    ==(false ? 4, None)
  }

  test("lift") {
    import hammerlab.option.lift._
    def lifted(x: Option[Int]) = x
    ===(lifted(123), Some(123))
  }
}
