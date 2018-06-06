package org.hammerlab

import hammerlab.option._

class OptionTest
  extends Suite {
  test("ops") {
    ==(true ? 4, Some(4))
    ==(false ? 4, None)
  }
}
