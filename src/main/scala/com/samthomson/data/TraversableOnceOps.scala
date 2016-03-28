package com.samthomson.data

import com.samthomson.data.mutable.DefaultDict

import scala.annotation.tailrec
import scala.collection.generic.{CanBuildFrom, CanBuild}
import scala.language.higherKinds
import scalaz.{Monoid, NonEmptyList}


object TraversableOnceOps
    extends GroupBy with Maxes with BalancedReduce with LazyMergeSort with TakeTo

trait GroupBy {
  implicit class GroupByOps[A](xs: TraversableOnce[A]) {
    def groupBy[K, Coll[_]](f: A => K)(implicit cb: CanBuild[A, Coll[A]]): Map[K, Coll[A]] = {
      xs.map(x => (f(x), x)).toMultimap
    }
  }

  implicit class MultimapOps[K, V](xs: TraversableOnce[(K, V)]) {
    def toMultimap[Coll[_]](implicit cb: CanBuild[V, Coll[V]]): Map[K, Coll[V]] = {
      val builders = DefaultDict((_: K) => cb.apply())
      for ((k, v) <- xs) {
        builders(k) += v
      }
      Map() ++ builders.mapValues(_.result())
    }
  }
}

trait Maxes {
  /**
   * Provides analogs of `maxBy`, `max`, `minBy`, `min` that
   * return _all_ maxes/mins in the case of ties.
   */
  implicit class MaxesOps[A](xs: TraversableOnce[A]) {
    def maxesBy[C, B >: C](f: A => C)(implicit ord: Ordering[B]): List[A] = {
      val (bests, _) = xs.foldLeft((List.empty[A], Option.empty[C])) {
        case ((_, None), y) => (List(y), Some(f(y))) // y is the first and best so far
        case ((oldBests, someOldF @ Some(oldF)), y) =>
          val fy = f(y)
          ord.compare(fy, oldF) match {
            case 0 => (y :: oldBests, someOldF) // tied. add it to the list
            case cmp if cmp > 0 => (List(y), Some(fy)) // new best!
            case _ => (oldBests, someOldF) // try harder, y
          }
      }
      bests
    }

    def minsBy[C, B >: C](f: A => C)(implicit ord: Ordering[B]): List[A] = xs.maxesBy[C, B](f)(ord.reverse)

    def maxes[B >: A](implicit ord: Ordering[B]): List[A] = maxesBy[A, B](identity)(ord)

    def mins[B >: A](implicit  ord: Ordering[B]): List[A] = xs.maxes(ord.reverse)
  }
}

trait BalancedReduce {
  implicit class BalancedReduceOps[A](xs: TraversableOnce[A]) {
    /**
     * Like `reduce`, but instead of reducing from the left (producing a left-leaning computation tree),
     * we reduce bottom up, producing a balanced computation tree.
     * This is useful when the runtime of `op` depends on the max size of its inputs.
     */
    def balancedReduce[B >: A](zero: B)(op: (B, B) => B): B = _balancedReduce(zero)(op)(xs)

    def balancedReduce[B >: A](implicit M: Monoid[B]): B = balancedReduce(M.zero)((x, y) => M.append(x, y))
  }

  /** keeps calling `reduceOnce` until there's only one element left */
  @tailrec
  final protected def _balancedReduce[B](zero: B)(op: (B, B) => B)(xs: TraversableOnce[B]): B = {
    xs.toStream match {
      case Stream.Empty => zero
      case x #:: Stream.Empty => x
      case xStream => _balancedReduce(zero)(op)(reduceOnce(zero)(op)(xStream))
    }
  }

  /**
   * Splits `xs` into adjacent pairs, and applies `op` to each.
   * Produces a stream half the size (rounding up) of `xs`.
   */
  final protected def reduceOnce[A, B >: A](zero: B)(op: (B, B) => B)(xs: Stream[A]): Stream[B] = {
    xs match {
      case x #:: y #:: rest => op(x, y) #:: reduceOnce(zero)(op)(rest)
      case _ => xs
    }
  }
}

trait LazyMergeSort {
  implicit class LazyMergeSortOps[A](xs: TraversableOnce[A])(implicit ord: Ordering[A]) {
    /**
     * Performs a lazy merge sort on `xs`.
     * `xs.lazySorted.take(k).toList` has runtime O(n + k * log(n)).
     */
    def lazySorted: Stream[A] = {
      import TraversableOnceOps.BalancedReduceOps
      xs.toList match {
        case Nil => Stream.Empty
        case a :: tl =>
          val runs = monotoneSeqs(tl, NonEmptyList(a), Nil, 0).map(_.stream)
          runs.balancedReduce(Stream.Empty)(lazyMerge[A])
      }
    }

    /**
     * Partitions `xs` into monotone non-decreasing or (reversed) non-increasing subsequences.
     * This speeds up merge sort, especially when the input is nearly sorted or nearly reverse sorted.
     * Strict, as you have to make one pass through the input anyway.
     */
    @tailrec
    final protected def monotoneSeqs(remaining: List[A],
                                     currentRun: NonEmptyList[A],
                                     result: List[NonEmptyList[A]],
                                     prevCmp: Int): List[NonEmptyList[A]] = {
      def newResult = (if (prevCmp < 0) currentRun.reverse else currentRun) :: result
      remaining match {
        case Nil => newResult
        case x :: rest =>
          val cmp = ord.compare(currentRun.head, x)
          if (cmp * prevCmp >= 0) {
            // keep the streak going.
            val newCmp = if (cmp == 0) prevCmp else cmp
            monotoneSeqs(rest, x <:: currentRun, result, newCmp)
          } else {
            // streak ended. wrap it up and start a new one.
            monotoneSeqs(rest, NonEmptyList(x), newResult, 0)
          }
      }
    }
  }

  /**
   * Lazily merges two sorted sequences into a new sorted stream.
   * `lazyMerge(as, bs).take(k).toList` has runtime O(k), assuming
   * we can dequeue from `as` and `bs` in constant time.
   */
  final def lazyMerge[A](as: Traversable[A],
                         bs: Traversable[A])
                        (implicit ord: Ordering[A]): Stream[A] = {
    if (as.isEmpty) {
      bs.toStream
    } else if (bs.isEmpty) {
      as.toStream
    } else {
      val (a, b) = (as.head, bs.head)
      if (ord.gt(a, b)) {
        b #:: lazyMerge(as, bs.tail)
      } else {
        a #:: lazyMerge(as.tail, bs)
      }
    }
  }
}

trait TakeTo {
  // (Scala's type inference can't handle `[A, This <: TraversableOnce[A]]`)
  implicit class TakeToOps[A, This](xs: This)(implicit ev: This <:< TraversableOnce[A]) {
    /** Returns the longest prefix of elements that don't satisfy `p`,
      * plus the first element that does satisfy `p` if one exists. */
    def takeTo[That](p: A => Boolean)(implicit bf: CanBuildFrom[This, A, That]): That = {
      import scala.util.control.Breaks.{breakable, break}
      val b = bf()
      breakable {
        for (x <- xs) {
          b += x
          if (p(x)) break()
        }
      }
      b.result()
    }
  }
}
