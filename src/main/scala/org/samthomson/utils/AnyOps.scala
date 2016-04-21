package org.samthomson.utils

object AnyOps extends Pipe with Unfold

trait Pipe {
  implicit class PipeOps[A](a: A) {
    /** Thrush operator. Applies `f` to `a`. */
    def |>[B](f: A => B): B = f(a)
  }
}

trait Unfold {
  implicit class UnfoldOps[S](start: S) {
    /**
      * Continually calls `step` on the current state, starting with `start`.
      * If `None`, then stops iterating.
      * If `Some(a, s)`, then sets current state to `s`, yields `a`, and continues iterating.
      */
    def unfold[B](step: S => Option[(B, S)]): Iterator[B] = new UnfoldIterator(start, step)

    /**
      * An `unfold` that just yields the state (including `start`).
      * Like `Iterator.iterate` except `step` also tells you when to stop (by returning `None`).
      * Equivalent to `Iterator.iterate(Option(start))(_.flatMap(step)).takeWhile(_.isDefined).map(_.get)`.
      */
    def iterate(step: S => Option[S]): Iterator[S] =
      Iterator.single(start) ++ new UnfoldIterator(start, step(_: S).map(next => (next, next)))
  }
}
