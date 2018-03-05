package org.hammerlab.math.syntax

import hammerlab.math.tolerance._

/**
 * Type-class for defining "fuzzy" {<,==,>}-style comparisons
 *
 * The contract is that, for any inputs `l` and `r`, exactly one of {<, ===, >} is true; {>=, <=, !==} are derived from
 * these in the expected way.
 *
 * Note that "equality" as defined here is not transitive! This should not be used to construct e.g. [[Ordering]]s.
 */
trait FuzzyCmp[L, R] {
  def <(l: L, r: R)(implicit ε: E): Boolean
  def >(l: L, r: R)(implicit ε: E): Boolean

  @inline def ===(l: L, r: R)(implicit ε: E): Boolean = ! >(l, r) && ! <(l, r)
  @inline def !==(l: L, r: R)(implicit ε: E): Boolean =   >(l, r) ||   <(l, r)
  @inline def  <>(l: L, r: R)(implicit ε: E): Boolean =   >(l, r) ||   <(l, r)

  @inline def  >=(l: L, r: R)(implicit ε: E): Boolean = ! <(l, r)
  @inline def   ≥(l: L, r: R)(implicit ε: E): Boolean = ! <(l, r)

  @inline def  <=(l: L, r: R)(implicit ε: E): Boolean = ! >(l, r)
  @inline def   ≤(l: L, r: R)(implicit ε: E): Boolean = ! >(l, r)
}

object FuzzyCmp {
  import Doubleish._
  implicit def forDoubleishes[L: Doubleish, R: Doubleish]: FuzzyCmp[L, R] =
    new FuzzyCmp[L, R] {
      @inline def <(l: L, r: R)(implicit ε: E): Boolean = ε.<(l.toDouble, r.toDouble)
      @inline def >(l: L, r: R)(implicit ε: E): Boolean = ε.>(l.toDouble, r.toDouble)
    }

  implicit class FuzzyCmpOps[L](l: L) {
    @inline def  >=[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp .>=(l, r)
    @inline def   ≥[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.  ≥(l, r)
    @inline def   >[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.  >(l, r)
    @inline def  <=[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp. <=(l, r)
    @inline def   ≤[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.  ≤(l, r)
    @inline def   <[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.  <(l, r)
    @inline def ===[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.===(l, r)
    @inline def !==[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.!==(l, r)
    @inline def  <>[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp. <>(l, r)
  }
}
