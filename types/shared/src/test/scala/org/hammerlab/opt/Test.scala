package org.hammerlab.opt

class Test
  extends hammerlab.Suite {
  def check(default: Opt[Int] = Non): Option[Int] = default
  test("opt") {
    ==(check(100), Some(100))
    ==(check(Som(100)), Some(100))
    ==(check(Non), None)
    ==(check(None), None)
    ==(check(Some(100)), Some(100))
  }
}
