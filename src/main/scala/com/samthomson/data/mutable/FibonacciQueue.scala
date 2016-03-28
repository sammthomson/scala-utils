package com.samthomson.data.mutable

import scala.collection.generic._
import scala.collection.{GenTraversableOnce, IndexedSeqLike, IterableLike, mutable => m}

/**
 * A mutable PriorityQueue interface built on top of a mutable FibonacciHeap
 * @author Sam Thomson (sthomson@cs.cmu.edu)
 */
class FibonacciQueue[A](implicit val ord: Ordering[A])
      extends m.AbstractIterable[A]
      with m.Iterable[A]
      with IterableLike[A, FibonacciQueue[A]]
      with Growable[A]
      with m.Builder[A, FibonacciQueue[A]]
      with Serializable
      with Cloneable {
  // FibonacciHeap pops low weights first. scala PQs pop high weights first
  protected[this] val heap: FibonacciHeap[A, A] = new FibonacciHeap[A, A]()(ord.reverse)

  override def head: A = heap.peekOption.getOrElse(throw new NoSuchElementException("queue empty"))

  def dequeue(): A = heap.pollOption.getOrElse(throw new NoSuchElementException("queue empty"))

  override def size: Int = heap.size

  override def iterator: Iterator[A] = heap.iterator.map(_.value)

  override def +=(e: A): this.type = { heap.add(e, e); this }

  def ++(xs: TraversableOnce[A]): FibonacciQueue[A] = {
    val result = new FibonacciQueue[A]()(ord)
    result ++= this
    result ++= xs
    result
  }

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[FibonacciQueue[A], B, That]): That = {
    val b = bf(repr)
    if (that.isInstanceOf[IndexedSeqLike[_, _]]) b.sizeHint(this, that.seq.size)
    b ++= thisCollection
    b ++= that.seq
    b.result()
  }

  override def ++=(xs: TraversableOnce[A]): this.type = {
    xs.foreach(+=)
    this
  }

  override def isEmpty: Boolean = size == 0

  override def result(): FibonacciQueue[A] = this

  override def clear(): Unit = heap.clear()

  override def seq: m.Iterable[A] = this

  protected[this] override def newBuilder = new FibonacciQueue[A]

  override def clone(): FibonacciQueue[A] = newBuilder ++= this.iterator
}
