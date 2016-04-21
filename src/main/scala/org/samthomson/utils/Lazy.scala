package org.samthomson.utils

/** A cell holding a lazily evaluated (and memoized) value */
class Lazy[+A] private (a: => A) {
  // keep track of whether or not force has been called yet, so
  // we can have a `toString` that doesn't force evaluation.
  private[this] var isEvaled: Boolean = false

  lazy val force: A = { isEvaled = true; a }

  override def toString: String = s"Lazy(${if (isEvaled) force else "?"})"
}
object Lazy {
  def apply[A](a: => A): Lazy[A] = new Lazy[A](a)
}
