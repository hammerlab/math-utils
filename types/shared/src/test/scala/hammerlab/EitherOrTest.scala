package hammerlab

class EitherOrTest
  extends Suite {
  test("either") {
    import hammerlab.either._
    val l = L(4)
    val r = R("a")

    val L(n) = l
    n should be(4)

    val R(a) = r
    a should be("a")
  }

  test("or") {
    import hammerlab.or._
    val l = L(4)
    val r = R("a")
    val b = Both(4, "a")

    val L(n) = l
    n should be(4)

    val R(a) = r
    a should be("a")

    val Both(m, s) = b
    m should be(4)
    s should be("a")

    val B(m2, s2) = b
    m2 should be(4)
    s2 should be("a")
  }
}
