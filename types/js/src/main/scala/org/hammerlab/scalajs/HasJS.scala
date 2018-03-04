package org.hammerlab.scalajs

trait HasJS {
  def JS [T](js : ⇒ T)(jvm: ⇒ T): T = js
  def JVM[T](jvm: ⇒ T)(js : ⇒ T): T = js

  def js [T](js : ⇒ T)(jvm: ⇒ T): T = js
  def jvm[T](jvm: ⇒ T)(js : ⇒ T): T = js
}
