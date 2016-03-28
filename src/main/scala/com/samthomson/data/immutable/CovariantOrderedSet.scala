package com.samthomson.data.immutable

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.Predef.{any2stringadd => _, _}


trait CovariantOrderedSet[S[+_]] {
  def isEmpty[A](xs: S[A]): Boolean
  def contains[A, B >: A](b: B)(xs: S[A])(implicit ord: Ordering[B]): Boolean
  def +[A, B >: A](b: B)(xs: S[A])(implicit ord: Ordering[B]): S[B]
  def toSeq[A](xs: S[A]): Seq[A]
}

object CovariantOrderedSet {
  implicit class Ops[S[+_], A](xs: S[A])(implicit ev: CovariantOrderedSet[S]) {
    def isEmpty: Boolean = ev.isEmpty(xs)
    def contains[B >: A](b: B)(implicit ord: Ordering[B]): Boolean = ev.contains(b)(xs)
    def +[B >: A](b: B)(implicit ord: Ordering[B]): S[B] = ev.+(b)(xs)
    def toSeq: Seq[A] = ev.toSeq(xs)
  }
}

sealed trait CovariantTreeSet[+A]

object CovariantTreeSet {
  import CovariantOrderedSet._

  private case object Leaf extends CovariantTreeSet[Nothing]
  private case class Node[+A](value: A,
                              left: CovariantTreeSet[A],
                              right: CovariantTreeSet[A]) extends CovariantTreeSet[A]

  def apply(): CovariantTreeSet[Nothing] = Leaf

  def apply[A](xs: A*)(implicit ord: Ordering[A]): CovariantTreeSet[A] = {
    xs.foldLeft(Leaf: CovariantTreeSet[A]) { (x, y) => x + y }
  }

  implicit val treeSetIsCovariantSet: CovariantOrderedSet[CovariantTreeSet] = new CovariantOrderedSet[CovariantTreeSet] {
    override def isEmpty[A](xs: CovariantTreeSet[A]): Boolean = xs == Leaf

    override def +[A, B >: A](b: B)(xs: CovariantTreeSet[A])(implicit ord: Ordering[B]): CovariantTreeSet[B] =
      xs match {
        case Leaf => Node(b, Leaf, Leaf)
        case Node(v, l, r) => ord.compare(b, v) match {
          case 0 => xs
          case c if c < 0 => Node(v, +(b)(l), r)
          case _ => Node(v, l, +(b)(r))
        }
      }

    @tailrec
    override def contains[A, B >: A](b: B)(xs: CovariantTreeSet[A])(implicit ord: Ordering[B]): Boolean =
      xs match {
        case Leaf => false
        case Node(v, l, r) => ord.compare(b, v) match {
          case 0 => true
          case c if c < 0 => contains(b)(l)
          case _ => contains(b)(r)
        }
      }

    override def toSeq[A](xs: CovariantTreeSet[A]): List[A] = loop(List(xs), Nil)

    @tailrec
    private def loop[A](path: List[CovariantTreeSet[A]],
                        acc: List[A]): List[A] =
      path match {
          case Nil => acc
          case Leaf :: tail => loop(tail, acc)
          case Node(v, l, r) :: tail => loop(l :: r :: tail, v :: acc)
      }
  }
}
