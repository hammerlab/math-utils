package org.hammerlab.scalajs

trait HasJSOps {
  implicit def makeJSOps[T] = HasJSOps.JSOps[T] _
}

object HasJSOps
  extends HasJS {
  implicit class JSOps[T](val l: T) extends AnyVal {
    def js(r: T): T = HasJSOps.jvm(l)(r)
  }
}
