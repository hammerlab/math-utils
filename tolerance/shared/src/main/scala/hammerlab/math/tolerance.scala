package hammerlab.math

import org.hammerlab.math.tolerance.Geometric.HasGeometric

trait tolerance
  extends Serializable
    with HasGeometric

object tolerance extends tolerance
