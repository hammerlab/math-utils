package org.hammerlab.math.syntax

import math.abs

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

  @inline def >= (l: L, r: R)(implicit ε: E): Boolean = ! <(l, r)
  @inline def ≥  (l: L, r: R)(implicit ε: E): Boolean = ! <(l, r)

  @inline def <= (l: L, r: R)(implicit ε: E): Boolean = ! >(l, r)
  @inline def ≤  (l: L, r: R)(implicit ε: E): Boolean = ! >(l, r)
}

object FuzzyCmp {
  implicit def forDoubleishes[L, R](implicit ldi: Doubleish[L], rdi: Doubleish[R]): FuzzyCmp[L, R] =
    new FuzzyCmp[L, R] {
      @inline def <(l: L, r: R)(implicit ε: E): Boolean = {
        val (ld, rd) = (ldi(l), rdi(r))
        if (ld < 0 && rd < 0)
          ld < rd * ε
        else
          ld * ε < rd
      }
      @inline def >(l: L, r: R)(implicit ε: E): Boolean = {
        val (ld, rd) = (ldi(l), rdi(r))
        if (ld < 0 && rd < 0)
          ld * ε > rd
        else
          ld > rd * ε
      }
    }

  implicit class FuzzyCmpOps[L](l: L) {
    @inline def >= [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.>= (l, r)
    @inline def ≥  [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.≥  (l, r)
    @inline def >  [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.>  (l, r)
    @inline def <= [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.<= (l, r)
    @inline def ≤  [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.≤  (l, r)
    @inline def <  [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.<  (l, r)
    @inline def ===[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.===(l, r)
    @inline def !==[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.!==(l, r)
    @inline def <> [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: E): Boolean = cmp.<> (l, r)
  }
}
