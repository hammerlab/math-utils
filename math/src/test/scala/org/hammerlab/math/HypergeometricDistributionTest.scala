package org.hammerlab.math

import org.scalactic.Equality
import org.hammerlab.test.Suite

import org.apache.commons.math3.distribution.{HypergeometricDistribution ⇒ ApacheHyperGeometricDistribution}

class HypergeometricDistributionTest
  extends Suite {

  var epsilon = 0.00001

  implicit val tolerance =
    new Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean =
        b match {
          case d: Double ⇒ a === d +- epsilon
          case _ ⇒ false
        }
    }

  implicit val approxBuffers =
    new Equality[Array[Double]] {
      override def areEqual(a: Array[Double], b: Any): Boolean =
        b match {
          case s: Array[Double] ⇒ a.size == s.size && a.zip(s).forall(t ⇒ t._1 === t._2)
          case _ ⇒ false
        }
    }

  def compareToApache(hgd: HypergeometricDistribution): Unit = {
    val N = hgd.N.toInt
    val K = hgd.K.toInt
    val n = hgd.n

    val apache = new ApacheHyperGeometricDistribution(N, K, n)

    hgd.pdf should ===(
      Array((0 to n).map(apache.probability): _*)
    )

    hgd.cdf should ===(
      Array((0 to n).map(apache.cumulativeProbability): _*)
    )
  }

  test("10-4-2") {
    val hgd = HypergeometricDistribution(10, 4, 2)

    hgd.pdf should ===(
      Array(
        1.0 / 3,
        8.0 / 15,
        2.0 / 15
      )
    )

    hgd.cdf should ===(
      Array(
        1.0 / 3,
        13.0 / 15,
        1
      )
    )

    List[Double](
      0,
      1.0 / 3 - epsilon,
      1.0 / 3,
      13.0 / 15 - epsilon,
      13.0 / 15,
      1 - epsilon,
      1
    )
    .map(hgd.invCDF(_)) should be(
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

  test("4-0-4") {
    val hgd = HypergeometricDistribution(4, 0, 4)
    hgd.cdf should ===(Array(1.0))
    hgd.pdf should ===(Array(1.0))
  }

  test("4-3-4") {
    val hgd = HypergeometricDistribution(4, 3, 4)
    hgd.pdf should ===(Array(0, 0, 0, 1.0))
    hgd.cdf should ===(Array(0, 0, 0, 1.0))
  }

  test("4-4-4") {
    val hgd = HypergeometricDistribution(4, 4, 4)
    hgd.pdf should ===(Array(0, 0, 0, 0, 1.0))
    hgd.cdf should ===(Array(0, 0, 0, 0, 1.0))
  }

  test("5-4-4") {
    val hgd = HypergeometricDistribution(5, 4, 4)
    hgd.pdf should ===(Array(0, 0, 0, 0.8, 0.2))
    hgd.cdf should ===(Array(0, 0, 0, 0.8, 1.0))
  }

  test("5000000000-4000000000-10") {
    val hgd = HypergeometricDistribution(5000000000L, 4000000000L, 10)

    hgd.pdf should be(
      Array(
        1.0239999631360417E-7,  //  0
        4.0959998894081015E-6,  //  1
        7.372799858073784E-5,   //  2
        7.864319899730114E-4,   //  3
        0.005505023958712356,   //  4
        0.026424115107515793,   //  5
        0.08808038393393967,    //  6
        0.20132659215099705,    //  7
        0.30198988830199097,    //  8
        0.26843545599999896,    //  9
        0.10737418215840859     // 10
      )
    )
  }
}
