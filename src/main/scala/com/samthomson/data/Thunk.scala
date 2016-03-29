package com.samthomson.data


/** A cell holding a lazily evaluated (and memoized) value */
class Thunk[+A] private (a: => A) {
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
    s"Thunk(${if (isSet) _a else "?"})"
  }
}
object Thunk {
  def apply[A](a: => A): Thunk[A] = new Thunk[A](a)
}
