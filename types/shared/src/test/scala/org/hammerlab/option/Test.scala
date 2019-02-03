package org.hammerlab.option

class Test
  extends hammerlab.Suite {
  test("ops") {
    ==(true ? 4, Some(4))
    ==(false ? 4, None)
  }
}
