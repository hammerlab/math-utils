package hammerlab

import org.hammerlab.collection.{ CanBuildFroms, Iter, Unfold }

trait collection
  extends   Iter.syntax
     with Unfold.syntax {
  trait cbf extends CanBuildFroms
  object cbf extends cbf
}
object collection extends collection
