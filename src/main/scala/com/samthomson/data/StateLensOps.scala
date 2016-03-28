/**
 * Just until this lands in monocle 2.2.0
 */
package com.samthomson.data

import scala.language.implicitConversions

import monocle.{POptional, PPrism, PLens}

import scalaz.{IndexedState, State}


object state extends StateSyntax


trait StateSyntax {
  implicit def toStateLensOps[S, T, A, B](lens: PLens[S, T, A, B]): StateLensOps[S, T, A, B] =
    new StateLensOps[S, T, A, B](lens)

  implicit def toStatePrismOps[S, T, A, B](prism: PPrism[S, T, A, B]): StatePrismOps[S, T, A, B] =
    new StatePrismOps[S, T, A, B](prism)

  implicit def toStateOptionalOps[S, T, A, B](optional: POptional[S, T, A, B]): StateOptionalOps[S, T, A, B] =
    new StateOptionalOps[S, T, A, B](optional)
}

final class StateLensOps[S, T, A, B](lens: PLens[S, T, A, B]) {
  /** transforms a [[PLens]] into a [[State]] */
  def gets: State[S, A] = State(s => (s, lens.get(s)))

  /** modify the value viewed through the lens and returns its *new* value */
  def mods(f: A => B): IndexedState[S, T, B] = {
    IndexedState(s => {
      val b = f(lens.get(s))
      (lens.set(b)(s), b)
    })
  }

  /** modify the value viewed through the lens and returns its *old* value */
  def modsOld(f: A => B): IndexedState[S, T, A] = gets.leftMap(lens.modify(f))

  /** set the value viewed through the lens and returns its *new* value */
  def assigns(b: B): IndexedState[S, T, B] = mods(_ => b)

  /** set the value viewed through the lens and returns its *old* value */
  def assignsOld(b: B): IndexedState[S, T, A] = modsOld(_ => b)
}


final class StatePrismOps[S, T, A, B](prism: PPrism[S, T, A, B]) {
  /** transforms a [[PPrism]] into a [[State]] */
  def gets: State[S, Option[A]] = State(s => (s, prism.getOption(s)))

  /** modify the value viewed through the lens and returns its *new* value */
  def mods(f: A => B): IndexedState[S, T, Option[B]] =
    IndexedState({
      prism.getOrModify(_).fold(
        t => (t, None),
        a => {
          val b = f(a)
          (prism.reverseGet(b), Some(b))
        })
    })

  /** modify the value viewed through the lens and returns its *old* value */
  def modsOld(f: A => B): IndexedState[S, T, Option[A]] = gets.leftMap(prism.modify(f))

  /** set the value viewed through the lens and returns its *new* value */
  def assigns(b: B): IndexedState[S, T, Option[B]] = mods(_ => b)

  /** set the value viewed through the lens and returns its *old* value */
  def assignsOld(b: B): IndexedState[S, T, Option[A]] = modsOld(_ => b)
}


final class StateOptionalOps[S, T, A, B](optional: POptional[S, T, A, B]) {
  /** transforms a [[POptional]] into a [[State]] */
  def gets: State[S, Option[A]] = State(s => (s, optional.getOption(s)))

  /** modify the value viewed through the lens and returns its *new* value */
  def mods(f: A => B): IndexedState[S, T, Option[B]] =
    IndexedState(s => {
      optional.getOrModify(s).fold(
        t => (t, None),
        a => {
          val b = f(a)
          (optional.set(b)(s), Some(b))
        })
    })

  /** modify the value viewed through the lens and returns its *old* value */
  def modsOld(f: A => B): IndexedState[S, T, Option[A]] = gets.leftMap(optional.modify(f))

  /** set the value viewed through the lens and returns its *new* value */
  def assigns(b: B): IndexedState[S, T, Option[B]] = mods(_ => b)

  /** set the value viewed through the lens and returns its *old* value */
  def assignsOld(b: B): IndexedState[S, T, Option[A]] = modsOld(_ => b)
}
