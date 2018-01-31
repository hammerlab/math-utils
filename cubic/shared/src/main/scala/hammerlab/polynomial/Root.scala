package hammerlab.polynomial

import org.hammerlab.math.format.showSuperscript

trait Root[+T] {
  def degree: Int
  def value: T
  override def toString: String = s"$value${showSuperscript.show(degree)}"
}

object Root {

}
