package org.hammerlab.math

import java.lang.Math.{ exp, log }

import hammerlab.math.utils.min

import scala.Array.fill

/**
 * Implementation of a hypergeometric distribution, modeled after
 * [[org.apache.commons.math3.distribution.HypergeometricDistribution]], but supporting [[Long]] parameters.
 *
 * @param N Population size.
 * @param K Number of successes.
 * @param n Number to sample.
 */
case class HypergeometricDistribution(N: Long, K: Long, n: Int) {

  val start = n - min(n, N - K)
  val end = min(K, n)

  // These will be filled with n+1 elements corresponding to the PDF and CDF values for k ∈ [0, n].
  val pdf = fill(end + 1)(0.0)
  val cdf = fill(end + 1)(0.0)

  // This will be set to the log of the binomial coefficient C(N, n), which is used multiple times in subsequent
  // calculations.
  var d = 0.0

  // logs of k!, for k in [0, n]
  val logBinomPartialSumsLo = fill(n + 1)(0.0)

  // logs of K! / (K - k)!, for k in [0, n]
  val logBinomPartialSumsK = fill(min(n, K) + 1)(0.0)

  // logs of (N - K)! / (N - K - k)!, for k in [0, n].
  val logBinomPartialSumsNK = fill(min(n, N - K) + 1)(0.0)

  // Compute log-arrays described above.
  1 to n foreach {
    k ⇒
      logBinomPartialSumsLo(k) = logBinomPartialSumsLo(k - 1) + log(k)

      if (k <= K)
      logBinomPartialSumsK(k) = logBinomPartialSumsK(k - 1) + log(K + 1 - k)

      if (k <= N - K)
        logBinomPartialSumsNK(k) += logBinomPartialSumsNK(k - 1) + log(N - K + 1 - k)

      d += log(N + 1 - k)
      d -= log(k)
  }

  // Compute PDF and CDF.
  start to end foreach {
    k ⇒
      val p1 = logBinomPartialSumsK(k) - logBinomPartialSumsLo(k)
      val p2 = logBinomPartialSumsNK(n - k) - logBinomPartialSumsLo(n - k)
      val v = exp(p1 + p2 - d)
      pdf(k) = v
      if (k == start)
        cdf(k) = v
      else
        cdf(k) = v + cdf(k - 1)
  }

  // Given a double x in [0, 1], binary-search the CDF to find the greatest integer k such that CDF(k) ≤ x.
  def invCDF(x: Double, start: Int = start, end: Int = end): Int = {
    if (start == end)
      start
    else {
      val mid = (start + end) / 2
      val c = cdf(mid)
      if (x <= c)
        invCDF(x, start, mid)
      else
        invCDF(x, mid + 1, end)
    }
  }
}
