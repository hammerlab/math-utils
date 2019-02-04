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

  test("dsl") {
    import hammerlab.opt._
    ===(123: ?[Int], Som(123))
    ===(?(123).filter(_ % 2 == 0), Non)
    ===(?(123).filter(_ % 3 == 0), Some(123))
  }

  test("std") {
    import hammerlab.opt.std._
    ===(123: Som[Int], Som(123))
    ===(123: Opt[Int], Som(123))
    ===(Som(123).filter(_ % 2 == 0), Non)
    ===(Som(123).filter(_ % 3 == 0), Some(123))
  }

  test("stdlib") {
    import hammerlab.opt.stdlib._
    ===(123: Some[Int], Some(123))
    ===(123: Option[Int], Some(123))
    ===(Some(123).filter(_ % 2 == 0), None)
    ===(Some(123).filter(_ % 3 == 0), Some(123))
  }
}
