package com.samthomson.utils


object AnyOps extends Pipe

trait Pipe {
  implicit class PipeOps[A](a: A) {
    /** Thrush operator. Applies `f` to `a`. */
    def |>[B](f: A => B): B = f(a)
  }
}
