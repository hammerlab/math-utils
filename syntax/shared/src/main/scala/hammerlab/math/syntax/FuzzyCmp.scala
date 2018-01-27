package hammerlab.math.syntax

trait FuzzyCmp[L, R] {
  def >= (l: L, r: R)(implicit ε: Tolerance): Boolean
  def >  (l: L, r: R)(implicit ε: Tolerance): Boolean
  def <= (l: L, r: R)(implicit ε: Tolerance): Boolean
  def <  (l: L, r: R)(implicit ε: Tolerance): Boolean
  def ===(l: L, r: R)(implicit ε: Tolerance): Boolean
  def !==(l: L, r: R)(implicit ε: Tolerance): Boolean = ! ===(l, r)
}

object FuzzyCmp {
  implicit def forDoubleishes[L, R](implicit ldi: Doubleish[L], rdi: Doubleish[R]): FuzzyCmp[L, R] =
    new FuzzyCmp[L, R] {
      override def >= (l: L, r: R)(implicit ε: Tolerance) = ldi(l) + ε >= rdi(r)
      override def >  (l: L, r: R)(implicit ε: Tolerance) = ldi(l) + ε >  rdi(r)
      override def <= (l: L, r: R)(implicit ε: Tolerance) = ldi(l) - ε <= rdi(r)
      override def <  (l: L, r: R)(implicit ε: Tolerance) = ldi(l) - ε <  rdi(r)
      override def ===(l: L, r: R)(implicit ε: Tolerance) = l <= r && l >= r
    }

  implicit class FuzzyCmpOps[L](l: L) {
    def >= [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: Tolerance): Boolean = cmp.>= (l, r)
    def >  [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: Tolerance): Boolean = cmp.>  (l, r)
    def <= [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: Tolerance): Boolean = cmp.<= (l, r)
    def <  [R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: Tolerance): Boolean = cmp.<  (l, r)
    def ===[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: Tolerance): Boolean = cmp.===(l, r)
    def !==[R](r: R)(implicit cmp: FuzzyCmp[L, R], ε: Tolerance): Boolean = cmp.!==(l, r)
  }
}
