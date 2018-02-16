package cubic.complex

import org.hammerlab.math.polynomial.Stats

class BigDecimalJVMTest
  extends BigDecimalTest {

  ε = 1e-16

  val sweep =
    results(
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
    results(
      absStats =
        Stats(
            n = 1200,
            μ = 2.230e-16,
            σ = 2.003e-16,
          max = 1.002e-15
        ),
      ratioStats =
        Stats(
            n = 1200,
            μ = 7.174e-16,
            σ = 2.580e-15,
          max = 4.010e-14
        )
    )



  val logNormalRandom =
    results(
      absStats =
        Stats(
            n = 1200,
            μ = 4.801e-16,
            σ = 6.307e-16,
          max = 4.885e-15
        ),
      ratioStats =
        Stats(
            n = 1200,
            μ = 7.497e-16,
            σ = 1.488e-15,
          max = 1.424e-14
        )
    )
}
