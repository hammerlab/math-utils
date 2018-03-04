package org.hammerlab.scalajs

trait HasJSOps {
  implicit def makeJSOps[T] = HasJSOps.JSOps[T] _
}

object HasJSOps
  extends HasJS {

  /**
   * For an arbitrary type, support using a different value in ScalaJS:
   *
   * {{{
   * "%.11f".format(66380.788125) == "66380.78812500000" js "66380.78812500001"
   * }}}
   */
  implicit class JSOps[T](val l: T) extends AnyVal {
    def js(r: T): T = HasJSOps.jvm(l)(r)
  }
}
