package org.hammerlab.math

import org.apache.commons.math3.distribution.{ HypergeometricDistribution ⇒ ApacheHyperGeometricDistribution }

class HypergeometricDistributionTest
  extends HypergeometricDistributionTestBase {

  val epsilon = 0.00001

  def compareToApache(hgd: HypergeometricDistribution): Unit = {
    val N = hgd.N.toInt
    val K = hgd.K.toInt
    val n = hgd.n

    val apache = new ApacheHyperGeometricDistribution(N, K, n)

    ==(
      hgd.pdf,
      Array((0 to n).map(apache.probability): _*)
    )

    ==(
      hgd.cdf,
      Array((0 to n).map(apache.cumulativeProbability): _*)
    )
  }

  test("10-4-2") {
    val hgd = HypergeometricDistribution(10, 4, 2)

    ==(
      hgd.pdf,
      Array(
        1.0 / 3,
        8.0 / 15,
        2.0 / 15
      )
    )

    ==(
      hgd.cdf,
      Array(
        1.0 / 3,
        13.0 / 15,
        1
      )
    )

    ==(
      List[Double](
        0,
        1.0 / 3 - epsilon,
        1.0 / 3,
        13.0 / 15 - epsilon,
        13.0 / 15,
        1 - epsilon,
        1
      )
      .map(hgd.invCDF(_)),
      List(
        0, 0, 1, 1, 2, 2, 2
      )
    )

    compareToApache(hgd)
  }

  test("500-100-10") {
    val hgd = HypergeometricDistribution(500, 100, 10)

    compareToApache(hgd)
  }
}
