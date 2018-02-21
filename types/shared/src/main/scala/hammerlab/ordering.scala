package hammerlab

trait ordering
  extends Serializable {
  object generic extends org.hammerlab.ordering.generic
}

object ordering extends ordering
