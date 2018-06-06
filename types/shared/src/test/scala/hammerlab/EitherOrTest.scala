package hammerlab

class EitherOrTest
  extends Suite {
  test("either") {
    import hammerlab.either._
    val l = L(4)
    val r = R("a")

    val L(n) = l
    ===(n, 4)

    val R(a) = r
    ===(a, "a")
  }

  test("or") {
    import hammerlab.or._
    val l = L(4)
    val r = R("a")
    val b = Both(4, "a")

    val L(n) = l
    ===(n, 4)

    val R(a) = r
    ===(a, "a")

    val Both(m, s) = b
    ===(m, 4)
    ===(s, "a")

    val B(m2, s2) = b
    ===(m2, 4)
    ===(s2, "a")
  }
}
