package hammerlab

class OrderingTest
  extends Suite {

  import hammerlab.ordering.generic._

  def check[T](elems: Seq[T])(implicit ord: Ordering[T]): Unit = {
    for {
      (e1, i1) ← elems.zipWithIndex
      (e2, i2) ← elems.zipWithIndex
      expected = (i1 - i2).signum
    } withClue(s"$e1 $e2 $expected: ") {
      ord.compare(e1, e2).signum should be( expected)
      ord.compare(e2, e1).signum should be(-expected)
    }
  }

  test("lists") {
    val seqs =
      for {
        size ← 0 to 10
      } yield
        (1 to size)

    check(seqs.map(_.toList))
    check(seqs.map(_.toArray))
    check(seqs.map(_.toVector))
    check(seqs.map(_.toSeq))
  }

  case class Foo(n: Int)
  case class Bar(foo: Foo, s: String)
  case class Baz(bar: Bar)

  test("case classes") {
    check(
      (1 to 10).map(Foo)
    )

    def bars(n: Int) =
      Seq(
        Bar(Foo(n), ""),
        Bar(Foo(n), "a"),
        Bar(Foo(n), "aa"),
        Bar(Foo(n), "x"),
        Bar(Foo(n), "xx")
      )

    check(
      bars(1) ++ bars(2) ++ bars(5)
    )

    check(
      (bars(1) ++ bars(2) ++ bars(5)).map(Baz)
    )
  }
}
