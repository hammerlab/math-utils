package org.hammerlab.collection

import hammerlab.collection._
import hammerlab.option._
import hammerlab.Suite

class CollectionTest
  extends Suite {
  test("canBuildFrom") {
    import hammerlab.collection.cbf._
    val ints = Vector(1, 2, 3)
    val arr = ints.map[Int, Array[Int]](_ * 2)
    ===(arr, Array(2, 4, 6))
    ===(arr.map[Int, Vector[Int]](_ * 2), Vector(4, 8, 12))
  }

  test("iter") {
    ==(     List(1, 2, 3)   list,     List(1, 2, 3) )
    ==(   Stream(1, 2, 3)   list,     List(1, 2, 3) )
    ==(   Vector(1, 2, 3)   list,     List(1, 2, 3) )
    ==( Iterator(1, 2, 3)   list,     List(1, 2, 3) )

    ==(     List(1, 2, 3) stream,   Stream(1, 2, 3) )
    ==(   Stream(1, 2, 3) stream,   Stream(1, 2, 3) )
    ==(   Vector(1, 2, 3) stream,   Stream(1, 2, 3) )
    ==( Iterator(1, 2, 3) stream,   Stream(1, 2, 3) )

    ==(     List(1, 2, 3)    vec,   Vector(1, 2, 3) )
    ==(   Stream(1, 2, 3)    vec,   Vector(1, 2, 3) )
    ==(   Vector(1, 2, 3)    vec,   Vector(1, 2, 3) )
    ==( Iterator(1, 2, 3)    vec,   Vector(1, 2, 3) )

    ==(     List(1, 2, 3)   iter, Iterator(1, 2, 3) )
    ==(   Stream(1, 2, 3)   iter, Iterator(1, 2, 3) )
    ==(   Vector(1, 2, 3)   iter, Iterator(1, 2, 3) )
    ==( Iterator(1, 2, 3)   iter, Iterator(1, 2, 3) )
  }

  test("unfold") {
    ===( 10 unfold { n ⇒ (n >  0) ? (n - 1) }, 10 to  0 by -1 toStream)
    ===( 10 unfold { n ⇒ (n > 10) ? (n - 1) }, 10 to 10       toStream)

    ===( 10 unfoldLeft { n ⇒ (n > 0) ? (n.toString, n-1)}, (10 to 1 by -1).map { _.toString }.toStream)
  }
}
