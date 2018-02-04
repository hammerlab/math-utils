package cubic

import org.hammerlab.math.polynomial
import org.hammerlab.math.polynomial.Real
import org.hammerlab.math.syntax.{ Arithmetic, Doubleish }

/**
 * This file is named ARoot.scala instead of Root.scala because it needs to come before "Cubic.scala" lexicographically,
 * due to [[https://github.com/scala/bug/issues/10222#issuecomment-333397589 scala/bug#10222]].
 */

sealed abstract class Root[D](val degree: Int)
  extends polynomial.Root[D] {
  def value: D
}

object Root {

  def apply[D](value: D, degree: Int): Root[D] =
    degree match {
      case 1 ⇒    Single(value)
      case 2 ⇒    Double(value)
      case 3 ⇒    Triple(value)
      case _ ⇒
        throw new IllegalArgumentException(
          s"Invalid degree: $degree"
        )
    }

  case class Single[D](value: D) extends Root[D](1)
  case class Double[D](value: D) extends Root[D](2)
  case class Triple[D](value: D) extends Root[D](3)

  def map[In, Out](f: In ⇒ Out): Root[In] ⇒ Root[Out] = {
    case Single(v) ⇒ Single(f(v))
    case Double(v) ⇒ Double(f(v))
    case Triple(v) ⇒ Triple(f(v))
  }

  implicit def arithmeticRootDbl[D](implicit a: Arithmetic.I[D]): Arithmetic[Root[D], D] =
    new Arithmetic[Root[D], D] {
      import Arithmetic.ArithmeticOps
      override def +(l: Root[D], r: D) = map[D, D](_ + r)(l)
      override def -(l: Root[D], r: D) = map[D, D](_ - r)(l)
      override def *(l: Root[D], r: D) = map[D, D](_ * r)(l)
      override def /(l: Root[D], r: D) = map[D, D](_ / r)(l)
    }
}
