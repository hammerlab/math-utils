package org.hammerlab.math.format

import cats.Show, Show.show

import scala.math.{ floor, log10, max, pow }

/**
 * Value-class representing a number of digits of precision to output numbers with
 */
class SigFigs(val n: Int) extends AnyVal {
  override def toString: String = n.toString
}

object SigFigs {

  implicit def apply(n: Int): SigFigs = new SigFigs(n)

  /**
   * Given a [[SigFigs]] and [[Exponent]]-format, render a [[Double]] to a [[String]]
   */
  implicit def showSigFigs(implicit
                           s: SigFigs,
                           ef: Exponent = Exponent.default): Show[Double] =
    new Show[Double] {
      override def show(d: Double): String =
        if (d < 0)
          '-' + show(-d)
        else {
          // Format the exponent as a string
          val exp = floor(log10(d)).toInt
          val expStr = ef(exp)

          // How long the string will be in scientific notation
          val scientificLength =
            s.n +                      // one digit left of decimal, the rest to the right
            (if (s.n > 1) 1 else 0) +  // decimal point
            expStr.length

          /** How long the string will be if the number is formatted directly to [[s.n]] sig-figs */
          val rawLength =
            if (exp >= 0)
              max(
                exp + 1, /** [[exp + 1]] digits, no decimal point */
                s.n + 1  /** [[s.n]] digits and one decimal point */
              )
            else
              -exp + 1 + s.n

          if (rawLength <= scientificLength) {
            // if printing the number directly is at least as short as scientific-notation, prefer the former
            s"%.${max(0, s.n - 1 - exp)}f".format(d)
          } else {
            // render with scientific notation
            val normalized = d / pow(10, exp)
            val fmt = s"%.${s.n - 1}f"
            val mantissa = fmt.format(normalized)
            s"$mantissa$expStr"
          }
        }
    }
}

