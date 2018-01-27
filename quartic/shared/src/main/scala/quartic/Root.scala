package quartic

import hammerlab.math.syntax.Arithmetic

sealed abstract class Root[D](val degree: Int) {
  def value: D
}

object Root {

  def apply[D](value: D, degree: Int): Root[D] =
    degree match {
      case 1 ⇒    Single(value)
      case 2 ⇒    Double(value)
      case 3 ⇒    Triple(value)
      case 4 ⇒ Quadruple(value)
      case _ ⇒
        throw new IllegalArgumentException(
          s"Invalid degree: $degree"
        )
    }

  def unapply[D](r: Root[D]): Some[(D, Int)] = Some((r.value, r.degree))

  case class Single[D](value: D) extends Root[D](1) {
    override def toString = value.toString
  }
  case class    Double[D](value: D) extends Root[D](2) {
    override def toString = s"$value²"
  }
  case class    Triple[D](value: D) extends Root[D](3) {
    override def toString = s"$value³"
  }
  case class Quadruple[D](value: D) extends Root[D](4) {
    override def toString = s"$value⁴"
  }

  def map[In, Out](f: In ⇒ Out): Root[In] ⇒ Root[Out] = {
    case    Single(v) ⇒    Single(f(v))
    case    Double(v) ⇒    Double(f(v))
    case    Triple(v) ⇒    Triple(f(v))
    case Quadruple(v) ⇒ Quadruple(f(v))
  }

  implicit def arithmeticRootDbl[D](implicit a: Arithmetic.I[D]): Arithmetic[Root[D], D] =
    new Arithmetic[Root[D], D] {
      import Arithmetic.makeReverseOps
      override def +(l: Root[D], r: D) = map[D, D](_ + r)(l)
      override def -(l: Root[D], r: D) = map[D, D](_ - r)(l)
      override def *(l: Root[D], r: D) = map[D, D](_ * r)(l)
      override def /(l: Root[D], r: D) = map[D, D](_ / r)(l)
    }
}
