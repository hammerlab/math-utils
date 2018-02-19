package quartic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJVMTest
  extends BigDecimalTest {

  val sweep =
    expected(
      absStats =
        Stats(
            n = 10920,
            μ = 4.457e-9,
            σ = 1.149e-8,
          max = 7.807e-8
        ),
      ratioStats =
        Stats(
            n = 10920,
            μ = 1.715e-9,
            σ = 5.299e-9,
          max = 7.097e-8
        ),
      numExpectedZeros = 1956
    )

  val random =
    expected(
      absStats =
        Stats(
            n = 2700,
            μ = 8.062e-8,
            σ = 1.382e-6,
          max = 3.899e-5
        ),
      ratioStats =
        Stats(
            n = 2700,
            μ = 1.129e-7,
            σ = 1.695e-6,
          max = 4.688e-5
        )
    )

  val logNormalRandom =
    expected(
      absStats =
        Stats(
            n = 2700,
            μ = 4.005e-8,
            σ = 2.919e-7,
          max = 7.549e-6
        ),
      ratioStats =
        Stats(
            n = 2700,
            μ = 2.331e-8,
            σ = 4.625e-8,
          max = 6.75e-7
        )
    )
}
