package cubic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJSTest
  extends BigDecimalTest {

  ε = 1e-16

  val sweep =
    expected(
      absStats =
        Stats(
            n = 990,
            μ = 7.235e-16,
            σ = 4.066e-16,
          max = 1.778e-15
        ),
      ratioStats =
        Stats(
            n = 990,
            μ = 3.539e-16,
            σ = 2.434e-16,
          max = 1.111e-15
        ),
      numExpectedZeros =
        264
    )

  val random =
    expected(
      absStats =
        Stats(
            n = 1200,
            μ = 2.229e-16,
            σ = 2.003e-16,
          max = 1.002e-15
        ),
      ratioStats =
        Stats(
            n = 1200,
            μ = 7.182e-16,
            σ = 2.580e-15,
          max = 4.010e-14
        )
    )



  val logNormalRandom =
    expected(
      absStats =
        Stats(
            n = 1200,
            μ = 4.806e-16,
            σ = 6.326e-16,
          max = 4.885e-15
        ),
      ratioStats =
        Stats(
            n = 1200,
            μ = 7.500e-16,
            σ = 1.491e-15,
          max = 1.442e-14
        )
    )
}
