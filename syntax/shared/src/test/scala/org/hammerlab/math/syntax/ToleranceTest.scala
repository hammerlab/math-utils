package org.hammerlab.math.syntax

import org.scalatest.{ FunSuite, Matchers }

import FuzzyCmp._

class ToleranceTest
  extends FunSuite
    with Matchers {

  implicit val ε: Tolerance = 1e-6

  def check(
      l: Double,
      r: Double,
      cmp: Cmp,
      includeReverse: Boolean = true
  ): Unit = {

    import cmp._

    assert(Cmp.lt(l, r) ==  LT      , s"($l <  $r)")
    assert(Cmp.le(l, r) ==  LT || EQ, s"($l <= $r)")
    assert(Cmp.gt(l, r) ==  GT      , s"($l >  $r)")
    assert(Cmp.ge(l, r) ==  GT || EQ, s"($l >= $r)")
    assert(Cmp.eq(l, r) ==  EQ      , s"($l == $r)")
    assert(Cmp.ne(l, r) == !EQ      , s"($l != $r)")

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

  /**
   * Define these outside of [[ToleranceTest]] because otherwise [[FuzzyCmpOps.=== ===]] conflicts with [[FunSuite.===]]
   */
  def gt[T: Doubleish](l: T, r: T)(implicit ε: Tolerance): Boolean = l  >  r
  def ge[T: Doubleish](l: T, r: T)(implicit ε: Tolerance): Boolean = l  >= r
  def eq[T: Doubleish](l: T, r: T)(implicit ε: Tolerance): Boolean = l === r
  def ne[T: Doubleish](l: T, r: T)(implicit ε: Tolerance): Boolean = l !== r
  def lt[T: Doubleish](l: T, r: T)(implicit ε: Tolerance): Boolean = l  <  r
  def le[T: Doubleish](l: T, r: T)(implicit ε: Tolerance): Boolean = l  <= r
}
