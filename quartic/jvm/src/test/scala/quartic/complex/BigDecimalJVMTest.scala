package quartic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJVMTest
  extends BigDecimalTest {

  val M = 2
  sweepTests(
    (4) →
      Stats(
        20,
        0,
        0,
        0
      ),
    (3,1) →
      Stats(
        80,
        1.3e-18,
        3.7e-18,
        1.6e-17
      ),
    (2,2) →
      Stats(
        40,
        0,
        0,
        0
      ),
    (2,1,1) →
      Stats(
        120,
        1.3e-16,
        2.7e-16,
        1.3e-15
      ),
    (1,1,1,1) →
      Stats(
        20,
        9.6e-17,
        1.2e-16,
        3.3e-16
      ),
    (2||1) →
      Stats(
        200,
        9.7e-17,
        1.8e-16,
        8.3e-16
      ),
    ((1,1)||1) →
      Stats(
        400,
        1.1e-16,
        1.1e-16,
        6.8e-16
      ),
    ||(2) →
      Stats(
        40,
        0,
        0,
        0
      ),
    ||(1,1) →
      Stats(
        180,
        9.3e-17,
        1.3e-16,
        5e-16
      )
  )

  val iterationsPerRootShape = 20
  gaussians(
    (4) →
      Stats(
        80,
        9.6e-34,
        1.4e-33,
        5.6e-33
      ),
    (3,1) →
      Stats(
        80,
        5.4e-12,
        5e-12,
        1.7e-11
      ),
    (2,2) →
      Stats(
        80,
        4.8e-17,
        6.1e-17,
        2.7e-16
      ),
    (2,1,1) →
      Stats(
        80,
        8.3e-16,
        2.6e-15,
        1.3e-14
      ),
    (1,1,1,1) →
      Stats(
        80,
        8.5e-16,
        2.1e-15,
        1.2e-14
      ),
    (2||1) →
      Stats(
        80,
        5.5e-16,
        1.8e-15,
        1.1e-14
      ),
    ((1,1)||1) →
      Stats(
        80,
        1.2e-16,
        1.1e-16,
        5.7e-16
      ),
    ||(2) →
      Stats(
        80,
        4.7e-17,
        8.2e-17,
        3.2e-16
      ),
    ||(1,1) →
      Stats(
        80,
        2.6e-16,
        2.2e-16,
        1e-15
      )
  )

  logNormals(
    (4) →
      Stats(
        80,
        3.2e-31,
        1.4e-30,
        6.3e-30
      ),
    (3,1) →
      Stats(
        80,
        5.8e-12,
        8.7e-12,
        4.5e-11
      ),
    (2,2) →
      Stats(
        80,
        8.3e-17,
        1.6e-16,
        7.7e-16
      ),
    (2,1,1) →
      Stats(
        80,
        2.1e-10,
        9.3e-10,
        4.4e-9
      ),
    (1,1,1,1) →
      Stats(
        80,
        4.2e-16,
        7.7e-16,
        3.8e-15
      ),
    (2||1) →
      Stats(
        80,
        1.9e-11,
        1.2e-10,
        7.5e-10
      ),
    ((1,1)||1) →
      Stats(
        80,
        1.4e-16,
        1.8e-16,
        9.3e-16
      ),
    ||(2) →
      Stats(
        80,
        2.9e-17,
        3.2e-17,
        1e-16
      ),
    ||(1,1) →
      Stats(
        80,
        5.4e-16,
        8.9e-16,
        4.8e-15
      )
  )
}
