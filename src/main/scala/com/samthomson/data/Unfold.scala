package com.samthomson.data

import scala.language.implicitConversions
import scala.collection.AbstractIterator


/**
  * Continually calls `step` on the current state, starting with `start`.
  * If `None`, then stops iterating.
  * If `Some(a, s)`, then sets current state to `s`, yields `a`, and continues iterating.
  */
class unfold[S, A] private(start: S, val step: S => Option[(A, S)]) extends AbstractIterator[A] {
  private[this] var _next = Thunk(step(start))

  override def hasNext: Boolean = _next.force.isDefined

  override def next(): A = _next.force match {
    case None => Iterator.empty.next()
    case Some((a, nextState)) =>
      _next = Thunk(step(nextState))
      a
  }
}
object unfold {
  def apply[S, A](start: S)(step: S => Option[(A, S)]): Iterator[A] = new unfold(start, step)

  /**
    * Special case where the state and the emission are the same.
    * Equivalent to `Iterator.iterate(Option(start))(_.flatMap(step)).drop(1).takeWhile(_.isDefined).map(_.get)`.
    */
  def states[S](start: S)(step: S => Option[S]): Iterator[S] =
    new unfold(start, (s: S) => step(s).map(next => (next, next)))
}
