package com.samthomson.utils


/** A cell holding a lazily evaluated (and memoized) value */
class Lazy[+A] private(a: => A) {
  private[this] var _a: A = _
  private[this] var isSet: Boolean = false

  def force: A = this.synchronized {
    // this could all be much simpler with a `lazy val`, but then
    // we couldn't have a nice `toString`.
    if (isSet) _a else {
      _a = a
      isSet = true
      _a
    }
  }

  override def toString: String = this.synchronized {
    s"Lazy(${if (isSet) _a else "?"})"
  }
}
object Lazy {
  def apply[A](a: => A): Lazy[A] = new Lazy[A](a)
}
