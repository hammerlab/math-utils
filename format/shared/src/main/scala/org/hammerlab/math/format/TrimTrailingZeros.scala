package org.hammerlab.math.format

sealed trait TrimTrailingZeros extends (String ⇒ String)
object TrimTrailingZeros {
  /** By default, trim trailing zeros (and decimal point, where applicable) */
  implicit case object yes extends TrimTrailingZeros {
    val regex = """^([^.]*)\.(\d*?)0+([^\d].*)?$""".r
    def apply(s: String): String =
      s match {
        case regex(beforeDot, afterDot, suffixGroup) ⇒
          val suffix = Option(suffixGroup).getOrElse("")
          if (afterDot.isEmpty)
            s"$beforeDot$suffix"
          else
            s"$beforeDot.$afterDot$suffix"
        case _ ⇒ s
      }
  }
  case object no extends TrimTrailingZeros {
    def apply(s: String): String = s
  }
  object implicits {
    /** Import this to preserve trailing zeros */
    implicit val no = TrimTrailingZeros.no
  }
}

