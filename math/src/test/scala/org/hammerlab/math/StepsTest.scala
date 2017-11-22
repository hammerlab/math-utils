package org.hammerlab.math

import hammerlab.math.Steps._
import org.hammerlab.test.Suite
import org.hammerlab.test.matchers.seqs.SeqMatcher.seqMatch

class StepsTest
  extends Suite {
  test("roundNumbers") {
    roundNumbers(200) should seqMatch(
      (  0 until  20) ++
      ( 20 until  50 by  2) ++
      ( 50 until 100 by  5) ++
      (100    to 200 by 10)
    )
  }

  test("geometricEvenSteps") {
    geometricSteps(1000, 20) should seqMatch(
      Seq(
        0, 1, 2, 3, 4, 5, 6, 9, 14, 21, 31, 46, 68, 99, 146, 215, 316, 464, 681, 999
      )
    )
  }
}
