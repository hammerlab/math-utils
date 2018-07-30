package org.hammerlab.math

abstract class HypergeometricDistributionTestBase
  extends hammerlab.Suite {
  test("4-0-4") {
    val hgd = HypergeometricDistribution(4, 0, 4)
    ==(hgd.cdf, Array(1.0))
    ==(hgd.pdf, Array(1.0))
  }

  test("4-3-4") {
    val hgd = HypergeometricDistribution(4, 3, 4)
    ==(hgd.pdf, Array(0, 0, 0, 1.0))
    ==(hgd.cdf, Array(0, 0, 0, 1.0))
  }

  test("4-4-4") {
    val hgd = HypergeometricDistribution(4, 4, 4)
    ==(hgd.pdf, Array(0, 0, 0, 0, 1.0))
    ==(hgd.cdf, Array(0, 0, 0, 0, 1.0))
  }

  test("5-4-4") {
    val hgd = HypergeometricDistribution(5, 4, 4)
    ==(hgd.pdf, Array(0, 0, 0, 0.8, 0.2))
    ==(hgd.cdf, Array(0, 0, 0, 0.8, 1.0))
  }

  test("5000000000-4000000000-10") {
    val hgd = HypergeometricDistribution(5000000000L, 4000000000L, 10)

    ==(
      hgd.pdf,
      Array(
        1.023999E-7,  //  0
        4.095999E-6,  //  1
        7.372799E-5,  //  2
        7.864319E-4,  //  3
        0.00550502,   //  4
        0.0264241,    //  5
        0.0880804,    //  6
        0.2013266,    //  7
        0.3019899,    //  8
        0.2684355,    //  9
        0.1073742     // 10
      )
    )
  }
}
