package quartic.complex

import org.hammerlab.math.polynomial.result.Stats

class BigDecimalJSTest
  extends BigDecimalTest {

  val M = 1
  sweepTests(
    (4) →
      Stats(
        12,
        0,
        0,
        0
      ),
    (3,1) →
      Stats(
        24,
        8.3e-35,
        1.9e-34,
        5e-34
      ),
    (2,2) →
      Stats(
        12,
        0,
        0,
        0
      ),
    (2,1,1) →
      Stats(
        12,
        1.1e-16,
        1.7e-16,
        4.4e-16
      ),
    (2||1) →
      Stats(
        36,
        6.8e-17,
        1.3e-16,
        3.9e-16
      ),
    ((1,1)||1) →
      Stats(
        36,
        1e-16,
        1.1e-16,
        3.5e-16
      ),
    ||(2) →
      Stats(
        12,
        0,
        0,
        0
      ),
    ||(1,1) →
      Stats(
        12,
        0,
        0,
        0
      )
  )

  val iterationsPerRootShape = 2
  gaussians(
    (4) →
      Stats(
        8,
        8.8e-34,
        8.4e-34,
        1.7e-33
      ),
    (3,1) →
      Stats(
        8,
        7.3e-12,
        6.8e-12,
        1.6e-11
      ),
    (2,2) →
      Stats(
        8,
        1.6e-16,
        1.1e-16,
        2.7e-16
      ),
    (2,1,1) →
      Stats(
        8,
        2.4e-16,
        3.9e-16,
        9e-16
      ),
    (1,1,1,1) →
      Stats(
        8,
        5.8e-16,
        3.2e-16,
        1.1e-15
      ),
    (2||1) →
      Stats(
        8,
        5e-17,
        5e-17,
        1.1e-16
      ),
    ((1,1)||1) →
      Stats(
        8,
        6.3e-17,
        3.7e-17,
        1.3e-16
      ),
    ||(2) →
      Stats(
        8,
        6.2e-18,
        6.4e-18,
        1.5e-17
      ),
    ||(1,1) →
      Stats(
        8,
        1.9e-16,
        7e-17,
        2.7e-16
      )
  )

  logNormals(
    (4) →
      Stats(
        8,
        5.3e-31,
        4.5e-31,
        9.7e-31
      ),
    (3,1) →
      Stats(
        8,
        4e-12,
        3.8e-12,
        8.9e-12
      ),
    (2,2) →
      Stats(
        8,
        5.4e-17,
        2.1e-17,
        7.5e-17
      ),
    (2,1,1) →
      Stats(
        8,
        7.1e-17,
        7.1e-17,
        1.5e-16
      ),
    (1,1,1,1) →
      Stats(
        8,
        1.4e-16,
        9.4e-17,
        2.9e-16
      ),
    (2||1) →
      Stats(
        8,
        1.7e-16,
        2.2e-16,
        5.3e-16
      ),
    ((1,1)||1) →
      Stats(
        8,
        1.5e-16,
        9.9e-17,
        3.6e-16
      ),
    ||(2) →
      Stats(
        8,
        0,
        0,
        0
      ),
    ||(1,1) →
      Stats(
        8,
        6.6e-16,
        6.7e-16,
        1.8e-15
      )
  )
}
