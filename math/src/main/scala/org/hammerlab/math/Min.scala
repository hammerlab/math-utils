package org.hammerlab.math

trait Min {
  def min(a: Int, b: Long): Int =
    if (a.toLong <= b)
      a
    else
      b.toInt

  def min(a: Long, b: Int): Int =
    if (a <= b.toLong)
      a.toInt
    else
      b
}
