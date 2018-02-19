package quartic.complex

import hammerlab.show._
import org.hammerlab.math.polynomial.result.{ Result, Stats }
import org.hammerlab.math.polynomial.{ Real, Stats, TestCase }
import spire.implicits._

class QuarticJVMTest
  extends QuarticTest {

  test("quad") {
    val result =
      Result(
        TestCase(
          Seq(
            Real(-0.2686) → 4
          ),
          Nil,
          -1.361
        )
      )

    implicitly[Show[Double]]
    println(show"${result → 1}")
  }

  test("ratio") {
    val result =
      Result(
        TestCase(
          Seq(
            Real(-0.005767) → 2,
            Real(-1.792) → 2
          ),
          Nil,
          -0.3042
        )
      )

    println(show"${result → 1}")
  }

  val sweep =
    expected(
      absStats =
        Stats(
            n = 10920,
            μ = 4.357e-9,
            σ = 1.147e-8,
          max = 7.807e-8
        ),
      ratioStats =
        Stats(
            n = 10920,
            μ = 1.656e-9,
            σ = 5.225e-9,
          max = 7.097e-8
        ),
      numExpectedZeros = 1860
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
