package org.hammerlab.implicits

/**
 * Wrapper for summoning an implicit and using it in a block:
 *
 * {{{
 * implicit def show[T]: Show[T] = {
 *   // expensive / non-idempotent derivation
 * }
 * wth[Show[T]] {
 *   implicit showT ⇒
 *   // do things that would otherwise re-derive a Show[T] instance multiple times
 * }
 * }}}
 */
case class `with`[T](implicit t: T) {
  def     &[U](fn: T ⇒ U): U = fn(t)
  def     →[U](fn: T ⇒ U): U = fn(t)
  def apply[U](fn: T ⇒ U): U = fn(t)
}

object wth {
  def apply[T](implicit t: T): `with`[T] = `with`[T]
}
