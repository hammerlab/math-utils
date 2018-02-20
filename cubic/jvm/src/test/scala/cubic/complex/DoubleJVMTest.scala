package cubic.complex

import org.hammerlab.math.polynomial.result.Stats

class DoubleJVMTest
  extends DoubleTest {
  val sweep =
    expected(
      Stats(
        19871,
        3.6e-9,
        3.16e-8,
        8.75e-7
      ),
      Stats(
        19871,
        4.57e-10,
        3.49e-9,
        7.86e-8
      )
    )

  val random =
    expected(
      Stats(
        20000,
        1.1e-6,
        2.89e-6,
        2.77e-5
      ),
      Stats(
        20000,
        1.38e-6,
        2.78e-6,
        1.23e-5
      )
    )

  val logNormalRandom =
    expected(
      Stats(
        20000,
        2.23e-6,
        7.74e-6,
        2.09e-4
      ),
      Stats(
        20000,
        1.38e-6,
        2.77e-6,
        1.15e-5
      )
    )
}
