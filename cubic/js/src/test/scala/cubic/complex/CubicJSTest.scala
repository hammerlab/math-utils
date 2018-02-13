package cubic.complex

import org.hammerlab.math.polynomial.{ Results, Stats }

class CubicJSTest
  extends CubicTest {

  val sweep =
    results(
      absStats =
        Stats(
            n = 2730,
            μ = 1.145e-8,
            σ = 3.166e-8,
          max = 2.406e-7
        ),
      ratioStats =
        Stats(
            n = 2730,
            μ = 3.313e-9,
            σ = 8.126e-9,
          max = 4.153e-8
        ),
      numExpectedZeros = 474
    )

  val random =
    results(
      absStats =
        Stats(
            n = 1200,
            μ = 3.503e-9,
            σ = 1.091e-8,
          max = 1.365e-7
        ),
      ratioStats =
        Stats(
            n = 1200,
            μ = 7.303e-9,
            σ = 4.634e-8,
          max = 8.86e-7
        )
    )

  val logNormalRandom =
    results(
      absStats =
        Stats(
            n = 1200,
            μ = 6.765e-9,
            σ = 2.441e-8,
          max = 2.604e-7
        ),
      ratioStats =
        Stats(
            n = 1200,
            μ = 4.254e-9,
            σ = 9.872e-9,
          max = 1.048e-7
        )
    )
}
