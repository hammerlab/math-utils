package org.hammerlab.math.tolerance

import org.scalatest.{ FunSuite, Matchers }

import hammerlab.math.tolerance._

class GeometricTest
  extends FunSuite
    with Matchers {

  implicit val ε: E = 1e-6

  def check(
      l: Double,
      r: Double,
      cmp: Cmp,
      includeReverse: Boolean = true
  ): Unit = {

    import cmp._

    assert((l <<< r) ==  LT      , s"($l <  $r)")
    assert((l <<= r) ==  LT || EQ, s"($l <= $r)")
    assert((l  ≤  r) ==  LT || EQ, s"($l ≤  $r)")
    assert((l >>> r) ==  GT      , s"($l >  $r)")
    assert((l >>= r) ==  GT || EQ, s"($l >= $r)")
    assert((l  ≥  r) ==  GT || EQ, s"($l >  $r)")
    assert((l === r) ==  EQ      , s"($l == $r)")
    assert((l !== r) == !EQ      , s"($l != $r)")
    assert((l  <> r) == !EQ      , s"($l <> $r)")

    if (includeReverse) {
      check( l,  r, cmp   , includeReverse = false)
      check(-l,  r, Cmp.LT, includeReverse = false)
      check( l, -r, Cmp.GT, includeReverse = false)
      check(
        -l, -r,
        cmp match {
          case Cmp.EQ ⇒ Cmp.EQ
          case Cmp.LT ⇒ Cmp.GT
          case Cmp.GT ⇒ Cmp.LT
        },
        includeReverse = false
      )
    }
  }

  import Cmp._
  test("barely fuzzy eq") {
    check(
      1,
      1 + 1e-6,
      EQ
    )
  }

  test("fuzzy eq") {
    check(
      1,
      1 + 1e-7,
      EQ
    )
  }

  test("barely not fuzzy eq") {
    check(
      1,
      1 + 1e-6 + 1e-10,
      LT
    )
  }

  test("not eq") {
    check(
      1,
      1 + 1e-5,
      LT
    )
  }

  test("really not eq") {
    check(
      1, 2, LT
    )
  }

  test("large barely eq") {
    check(
      1.23e10,
      1.2300012299999998e10,
      EQ
    )
  }

  test("large barely ne") {
    check(
      1.23e10,
      1.230001230123e10,
      LT
    )
  }

  test("small barely eq") {
    check(
      1.23e-10,
      1.2300012299999998e-10,
      EQ
    )
  }

  test("small barely ne") {
    check(
      1.23e-10,
      1.230001230123e-10,
      LT
    )
  }
}

sealed abstract class Cmp(
    val LT: Boolean = false,
    val EQ: Boolean = false,
    val GT: Boolean = false
)
object Cmp {

  case object LT extends Cmp(LT = true)
  case object EQ extends Cmp(EQ = true)
  case object GT extends Cmp(GT = true)

  type D = Double
  /**
   * Define these outside of [[GeometricTest]] because otherwise [[Geometric.Ops.===]] conflicts with [[FunSuite.===]]
   */
  def gt(l: D, r: D)(implicit ε: E): Boolean = l >>> r
  def ge(l: D, r: D)(implicit ε: E): Boolean = l >>= r
  def eq(l: D, r: D)(implicit ε: E): Boolean = l === r
  def ne(l: D, r: D)(implicit ε: E): Boolean = l !== r
  def lt(l: D, r: D)(implicit ε: E): Boolean = l <<< r
  def le(l: D, r: D)(implicit ε: E): Boolean = l <<= r
}
