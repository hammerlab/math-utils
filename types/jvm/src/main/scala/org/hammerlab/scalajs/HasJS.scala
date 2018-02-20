package org.hammerlab.scalajs

trait HasJS {
  def JS [T](js : ⇒ T)(jvm: ⇒ T): T = jvm
  def JVM[T](jvm: ⇒ T)(js : ⇒ T): T = jvm

  def js [T](js : ⇒ T)(jvm: ⇒ T): T = jvm
  def jvm[T](jvm: ⇒ T)(js : ⇒ T): T = jvm
}
