package org.hammerlab.str

import cats.implicits._
import hammerlab.str._

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
      Seq[Str](
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

    ==(
      'abc: String,
      "abc"
    )
  }
}
