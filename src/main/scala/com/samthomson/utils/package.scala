package com.samthomson

package object utils {
  /**
    * Continually calls `step` on the current state, starting with `start`.
    * If `None`, then stops iterating.
    * If `Some(a, s)`, then sets current state to `s`, yields `a`, and continues iterating.
    */
  def unfold[S, A](start: S)(step: S => Option[(A, S)]): Iterator[A] =
    new UnfoldIterator(start, step)

  /**
    * Special case of unfold where the state and the emission are the same.
    * Equivalent to `Iterator.iterate(Option(start))(_.flatMap(step)).drop(1).takeWhile(_.isDefined).map(_.get)`.
    */
  def unfoldStates[S](start: S)(step: S => Option[S]): Iterator[S] =
    new UnfoldIterator(start, (s: S) => step(s).map(next => (next, next)))
}
