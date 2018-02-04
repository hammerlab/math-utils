package org.hammerlab.math.polynomial

import org.hammerlab.math.format.showSuperscript
import org.hammerlab.math.syntax.Doubleish

trait Root[+T] {
  def degree: Int
  def value: T
  override def toString: String = s"$value${showSuperscript.show(degree)}"
}

object Root {
  def doubleish[T](implicit d: Doubleish[T]): Doubleish[Root[T]] =
    new Doubleish[Root[T]] {
      override def apply(t: Root[T]): scala.Double = d(t.value)
    }
}
