package com.samthomson.data.immutable

import scala.language.higherKinds


trait IsCovariantSet[S[+_]] {
  def isEmpty[A](xs: S[A]): Boolean
  def contains[A](b: Any)(xs: S[A]): Boolean
  def +[A, B >: A](b: B)(xs: S[A]): S[B]
  def -[A, B >: A](b: B)(xs: S[A]): S[B]
  def toSeq[A](xs: S[A]): Seq[A]
}

object IsCovariantSet {
  implicit class Ops[S[+_], A](xs: S[A])(implicit ev: IsCovariantSet[S]) {
    def isEmpty: Boolean = ev.isEmpty(xs)
    def contains(b: Any): Boolean = ev.contains(b)(xs)
    def +[B >: A](b: B): S[B] = ev.+(b)(xs)
    def -[B >: A](b: B): S[B] = ev.-(b)(xs)
    def toSeq: Seq[A] = ev.toSeq(xs)
  }
}

/** @param data will always be a Set[A] */
class CovariantSet[+A] private (val data: Iterable[A])

object CovariantSet {
  def apply[A](xs: A*): CovariantSet[A] = new CovariantSet(xs.toSet)

  implicit class CovariantSetAssoc[A](xs: TraversableOnce[A]) {
    def toCovariantSet: CovariantSet[A] = new CovariantSet(xs.toSet)
  }

  implicit val covariantSetIsCovariantSet: IsCovariantSet[CovariantSet] =
    new IsCovariantSet[CovariantSet] {
      override def isEmpty[A](xs: CovariantSet[A]): Boolean = xs.data.isEmpty

      override def contains[A](b: Any)(xs: CovariantSet[A]): Boolean = xs.data.toSet.contains(b)

      override def +[A, B >: A](b: B)(xs: CovariantSet[A]): CovariantSet[B] = new CovariantSet(xs.data.toSet + b)

      override def -[A, B >: A](b: B)(xs: CovariantSet[A]): CovariantSet[B] = new CovariantSet(xs.data.toSet - b)

      override def toSeq[A](xs: CovariantSet[A]): Seq[A] = xs.data.toSeq
    }
}
