package com.samthomson.data

import scala.language.implicitConversions
import scala.collection.AbstractIterator
import scalaz.Functor

/** A cell holding a lazily evaluated value */
class Thunk[+A](a: => A) {
  lazy val force: A = a
}
object Thunk {
  def apply[A](a: => A): Thunk[A] = new Thunk[A](a)

  class Thunktor extends Functor[Thunk] {
    override def map[A, B](fa: Thunk[A])(f: (A) => B): Thunk[B] = Thunk(f(fa.force))
  }
}

object StreamFunctions {
  def unfold[A, B](start: A)(step: A => Option[(B, A)]): Stream[B] = {
    step(start) match {
      case None  => Stream.empty
      case Some((b, a)) => b #:: unfold(a)(step)
    }
  }
}

object IteratorFunctions {
  def unfold[S, A](start: S)(step: S => Option[(A, S)]): Iterator[A] = new UnfoldIterator(start, step)

  class UnfoldIterator[S, A](start: S, val step: S => Option[(A, S)]) extends AbstractIterator[A] {
    private var _next = Thunk(step(start))

    override def hasNext: Boolean = _next.force.isDefined

    override def next(): A = _next.force match {
      case None => Iterator.empty.next()
      case Some((b, nextState)) => _next = Thunk(step(nextState)); b
    }
  }
}
