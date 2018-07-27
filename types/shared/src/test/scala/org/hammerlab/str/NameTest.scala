package org.hammerlab.str

import cats.implicits._

class NameTest
  extends hammerlab.Suite {
  test("implicits") {
    ==(
      Seq[Name](
        'abc,
        "def"
      )
      .mkString(","),
      "abc,def"
    )

    ==(
      Seq[Name](
        'abc,
        "def"
      )
      .map(_.show)
      .mkString(","),
      "abc,def"
    )

    ==(
      Name('abc): String,
      "abc"
    )
  }
}
