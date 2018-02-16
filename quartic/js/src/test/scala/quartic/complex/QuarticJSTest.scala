package quartic.complex

import org.hammerlab.math.polynomial.Stats

class QuarticJSTest
  extends QuarticTest {

  val sweep =
    results(
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
      numExpectedZeros = 1968
    )

  val random =
    results(
      absStats =
        Stats(
            n = 2700,
            μ = 8.051e-8,
            σ = 1.382e-6,
          max = 3.899e-5
        ),
      ratioStats =
        Stats(
            n = 2700,
            μ = 1.123e-7,
            σ = 1.695e-6,
          max = 4.688e-5
        )
    )

  val logNormalRandom =
    results(
      absStats =
        Stats(
            n = 2700,
            μ = 3.798e-8,
            σ = 1.964e-7,
          max = 4.749e-6
        ),
      ratioStats =
        Stats(
            n = 2700,
            μ = 3.057e-8,
            σ = 1.649e-7,
          max = 4.37e-6
        )
    )
}
