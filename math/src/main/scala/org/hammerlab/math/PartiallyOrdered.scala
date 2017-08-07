package org.hammerlab.math

/**
 * Fork of [[scala.math.PartiallyOrdered]] trait that inherits [[Any]], making it a universal trait suitable for
 * inheritance by value-classes. See https://issues.scala-lang.org/browse/SI-10128.
 *
 * A class for partially ordered data.
 *
 * Forked from the Scala standard-lib in order to make it a universal trait, for mixing-in to value-classes.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 23/04/2004
 */
trait PartiallyOrdered[+A] extends Any {

  /** Result of comparing `'''this'''` with operand `that`.
   *  Returns `None` if operands are not comparable.
   *  If operands are comparable, returns `Some(x)` where
   *  - `x < 0`    iff   `'''this''' &lt; that`
   *  - `x == 0`   iff   `'''this''' == that`
   *  - `x > 0`    iff   `'''this''' &gt; that`
   */
  def tryCompareTo [B >: A : PartiallyOrdered](that: B): Option[Int]

  def <  [B >: A : PartiallyOrdered](that: B): Boolean =
    this tryCompareTo that match {
      case Some(x) if x < 0 ⇒ true
      case _ ⇒ false
    }
  def >  [B >: A : PartiallyOrdered](that: B): Boolean =
    this tryCompareTo that match {
      case Some(x) if x > 0 ⇒ true
      case _ ⇒ false
    }
  def <= [B >: A : PartiallyOrdered](that: B): Boolean =
    this tryCompareTo that match {
      case Some(x) if x <= 0 ⇒ true
      case _ ⇒ false
    }
  def >= [B >: A : PartiallyOrdered](that: B): Boolean =
    this tryCompareTo that match {
      case Some(x) if x >= 0 ⇒ true
      case _ ⇒ false
    }
}
