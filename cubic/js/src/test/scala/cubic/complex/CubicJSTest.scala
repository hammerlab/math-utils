package cubic.complex

import org.hammerlab.math.polynomial.{ Results, Stats }

class CubicJSTest
  extends CubicTest {

  val sweep =
    results(
      absStats =
        Stats(
            n = 2730,
          max = 2.406e-7,
            μ = 1.145e-8,
            σ = 3.166e-8
        ),
      ratioStats =
        Stats(
            n = 2730,
          max = 4.153e-8,
            μ = 3.313e-9,
            σ = 8.126e-9
        ),
      numExpectedZeros = 474
    )

  val random =
    results(
      absStats =
        Stats(
            n = 1200,
          max = 1.365e-7,
            μ = 3.503e-9,
            σ = 1.091e-8
        ),
      ratioStats =
        Stats(
            n = 1200,
          max = 8.86e-7,
            μ = 7.303e-9,
            σ = 4.634e-8
        )
    )
}
