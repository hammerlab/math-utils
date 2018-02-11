package org.hammerlab.stats

import hammerlab.show._

sealed trait Percentile
object Percentile {
  /**
   * A [[Show]] implementation that is always 6 characters long or less (up to 1e-9 and 1-1e-9)
   */
  implicit val showPercentile: Show[Percentile] =
    Show {
      case Lo(exp) ⇒
        if (exp <= 3)
          s".${"0" * (exp - 1)}1"
        else
          s"1e-$exp"
      case Mid(value) ⇒ ".%02d".format(value)
      case Hi(exp) ⇒
        if (exp <= 5)
          s".${"9" * exp}"
        else
          s"1-1e-$exp"
    }

  val ord = implicitly[Ordering[Int]]
  implicit val ordering: Ordering[Percentile] =
    new Ordering[Percentile] {
      override def compare(x: Percentile, y: Percentile): Int =
        (x, y) match {
          case ( Lo(l),  Lo(r)) ⇒ ord.compare(r, l)
          case (Mid(l), Mid(r)) ⇒ ord.compare(l, r)
          case ( Hi(l),  Hi(r)) ⇒ ord.compare(l, r)
          case ( Lo(_),     _ ) ⇒ -1
          case (    _ ,  Hi(_)) ⇒ -1
          case ( Hi(_),     _ ) ⇒  1
          case (    _ ,  Lo(_)) ⇒  1
        }
    }
}
case class  Lo(  exp: Int) extends Percentile
case class Mid(value: Int) extends Percentile
case class  Hi(  exp: Int) extends Percentile
