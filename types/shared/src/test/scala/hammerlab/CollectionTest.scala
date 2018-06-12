package hammerlab

import hammerlab.collection._

class CollectionTest
  extends Suite {
  test("canBuildFrom") {
    val ints = Vector(1, 2, 3)
    val arr = ints.map[Int, Array[Int]](_ * 2)
    ==(arr, Array(2, 4, 6))
    ==(arr.map[Int, Vector[Int]](_ * 2), Vector(4, 8, 12))
  }
}
