package cubic.complex

import spire.implicits._

abstract class DoubleTest
  extends CubicTest[Double] {
  override val casePrintInterval = 1000
  val M = 15
  val iterationsPerRootShape = 5000
}
