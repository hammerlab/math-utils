package hammerlab.math

import org.hammerlab.math.format.HasSigFigs

trait sigfigs
  extends HasSigFigs
    with Serializable

object sigfigs extends sigfigs
