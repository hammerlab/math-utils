package org.hammerlab.str

import cats.Show

case class Name(override val toString: String)
object Name {
  implicit def   apply(symbol: Symbol): Name = new Name(symbol.name)
  implicit def fromStr(string: String): Name = new Name(string     )
  implicit def unwrap(n: Name): String = n.toString
  implicit val show: Show[Name] =
    new Show[Name] {
      def show(t: Name): String = t.toString
    }
}
