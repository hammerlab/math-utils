package hammerlab.math

import org.hammerlab.math.format.Exponent

trait exponent
  extends Exponent.format
     with Exponent.instances

object exponent extends exponent
