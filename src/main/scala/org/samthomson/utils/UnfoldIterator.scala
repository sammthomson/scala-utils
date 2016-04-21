package org.samthomson.utils

import scala.language.implicitConversions
import scala.collection.AbstractIterator


/**
  * Continually calls `step` on the current state, starting with `start`.
  * If `None`, then stops iterating.
  * If `Some(a, s)`, then sets current state to `s`, yields `a`, and continues iterating.
  */
class UnfoldIterator[S, A](start: S, val step: S => Option[(A, S)]) extends AbstractIterator[A] {
  private[this] var _next = Lazy(step(start))

  override def hasNext: Boolean = _next.force.isDefined

  override def next(): A = _next.force match {
    case None => Iterator.empty.next()
    case Some((a, nextState)) =>
      _next = Lazy(step(nextState))
      a
  }
}
