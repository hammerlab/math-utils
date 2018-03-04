package org.hammerlab.math

trait HasBinomial
  extends Serializable {
  object binomial {
    def apply(n: Int, k: Int): Long =
      if (k > n || k < 0 || n < 0)
        0
      else if (k > n - k)
        apply(n, n - k)
      else {
        var b = 1
        var i = 1
        var m = n
        while (i <= k) {
          b = b * m / i
          i += 1
          m -= 1
        }
        b
      }
  }
}

object HasBinomial extends HasBinomial
