package org.hammerlab

import hammerlab.option._

class OptionTest
  extends Suite {
  test("ops") {
    ==(true ? 4, Some(4))
    ==(false ? 4, None)
  }

  def check(default: Opt[Int] = Non): Option[Int] = default
  test("opt") {
    ==(check(100), Some(100))
    ==(check(Som(100)), Some(100))
    ==(check(Non), None)
    ==(check(None), None)
    ==(check(Some(100)), Some(100))
  }
}
