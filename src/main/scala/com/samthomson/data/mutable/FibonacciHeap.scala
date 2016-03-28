package com.samthomson.data.mutable

import scala.annotation.tailrec
import scala.collection.{AbstractIterator, mutable => m}
import scala.language.implicitConversions
import FibonacciHeap._

/**
 * A mutable Fibonacci heap (due to Fredman and Tarjan).
 *
 * This implementation was inspired and informed by those of
 * Nathan L. Fiedler
 * (https://github.com/nlfiedler/graphmaker/blob/990227c766a9891be1d4669c582975f7c1a4db87/core/src/com/bluemarsh/graphmaker/core/util/FibonacciHeap.java),
 * and
 * Keith Schwarz (htiek@cs.stanford.edu)
 * (http://www.keithschwarz.com/interesting/code/?dir=fibonacci-heap),
 * but rewritten from scratch by Sam Thomson (sthomson@cs.cmu.edu).
 *
 * @tparam V the type of the values stored in the heap
 * @tparam P the type of the priorities
 */
class FibonacciHeap[V, P](implicit val ord: Ordering[P]) extends Iterable[Entry[V, P]] {
  private type E = Entry[V, P]

  private var oMinEntry: Option[Entry[V, P]] = None
  private var len = 0

  // we're going to use `None` as a negative-infinity--like priority
  private val noneFirst: Ordering[Option[P]] = Ordering.Option(ord)
  import noneFirst._ // can just use bare '<', '<=', etc. now

  /** Removes all elements from the heap. Runtime: O(1) */
  def clear() {
    oMinEntry = None
    len = 0
  }

  /** Decreases the priority for an entry. Runtime: O(1) (amortized) */
  def decreasePriority(entry: Entry[V, P], newPriority: P) {
    val oPriority = Some(newPriority)
    require(oPriority <= entry.priority, "Cannot increase priority")
    entry.priority = oPriority
    assert(oMinEntry.isDefined)
    val minEntry = oMinEntry.get
    for (parent <- entry.oParent if parent.priority > oPriority) cutAndMakeRoot(entry)
    if (oPriority < minEntry.priority) {
      oMinEntry = Some(entry)
    }
  }

  /**
   * Deletes `entry` from the heap. The heap will be consolidated, if necessary.
   * Runtime: O(log n) amortized
   */
  def remove(entry: Entry[V, P]) {
    entry.priority = None
    cutAndMakeRoot(entry)
    oMinEntry = Some(entry)
    pollOption
  }

  /** Returns true if the heap is empty, false otherwise. Runtime: O(1) */
  override def isEmpty: Boolean = oMinEntry.isEmpty

  /**
   * Inserts a new entry into the heap and returns the entry.
   * No heap consolidation is performed.
   * Runtime: O(1)
   */
  def add(value: V, priority: P): Entry[V, P] = {
    val result = new Entry[V, P](value, Some(priority))
    oMinEntry = mergeLists(Some(result), oMinEntry)
    len += 1
    result
  }

  /**
   * Returns the entry with the minimum priority, or absent if empty.
   * Runtime: O(1)
   */
  def peekOption: Option[Entry[V, P]] = oMinEntry

  /**
   * Removes the smallest element from the heap. This will cause
   * the trees in the heap to be consolidated, if necessary.
   * Runtime: O(log n) amortized
   */
  def pollOption: Option[V] = oMinEntry map { minEntry =>
    minEntry.children foreach { _.oParent = None }
    mergeLists(oMinEntry, minEntry.oFirstChild)
    if (len == 1) {
      oMinEntry = None
    } else {
      val next = minEntry.next
      unlinkFromNeighbors(minEntry)
      oMinEntry = Some(consolidate(next))
    }
    len -= 1
    minEntry.value
  }

  /**
   * Returns the number of elements in the heap.
   * Runtime: O(1)
   */
  override def size: Int = len

  /** Returns every entry in this heap, in no particular order. */
  def iterator: Iterator[Entry[V, P]] = oMinEntry match {
    case None => Iterator.empty
    case Some(root) => new DepthFirstEntryIterator[V, P](root)
  }

  /**
   * Merge two doubly-linked circular lists, given a pointer into each.
   * Return the smaller of the two arguments.
   */
  private def mergeLists(oA: Option[E], oB: Option[E]): Option[E] = {
    if (oA.isEmpty) return oB
    if (oB.isEmpty) return oA
    val a = oA.get
    val b = oB.get
    val aOldNext = a.next
    a.next = b.next
    a.next.previous = a
    b.next = aOldNext
    b.next.previous = b
    if (a.priority < b.priority) oA else oB
  }

  /**
   * Cuts this entry from its parent and adds it to the root list, and then
   * does the same for its parent, and so on up the tree.
   * Runtime: O(log n)
   */
  @tailrec
  private def cutAndMakeRoot(entry: E) {
    entry.oParent match {
      case None => ()
      case Some(parent) =>
        parent.degree -= 1
        entry.isMarked = false
        assert(parent.oFirstChild.isDefined)
        if (parent.oFirstChild.get == entry) {
          if (parent.degree == 0) {
            parent.oFirstChild = None
          } else {
            parent.oFirstChild = Some(entry.next)
          }
        }
        entry.oParent = None
        unlinkFromNeighbors(entry)
        mergeLists(Some(entry), oMinEntry)
        if (parent.oParent.isDefined) {
          if (parent.isMarked) {
            cutAndMakeRoot(parent)
          } else {
            parent.isMarked = true
          }
        }
    }
  }

  /** Attaches `entry` as a child of `parent`. Returns `parent`. */
  private def setParent(entry: E, parent: E): E = {
    unlinkFromNeighbors(entry)
    entry.oParent = Some(parent)
    parent.oFirstChild = mergeLists(Some(entry), parent.oFirstChild)
    parent.degree += 1
    entry.isMarked = false
    parent
  }

  /**
   * Consolidates the trees in the heap by joining trees of equal
   * degree until there are no more trees of equal degree in the
   * root list. Returns the new minimum root.
   * Runtime: O(log n) (amortized)
   */
  private def consolidate(someRoot: E): E = {
    val entryOrd = Entry.ordering[V, P](ord)
    var minRoot = someRoot
    val rootsByDegree = m.Map[Int, E]()
    for (currRoot <- getCycle(Some(someRoot))) {
      var mergedRoot = currRoot
      var degree = currRoot.degree
      while (rootsByDegree.contains(degree)) {
        val oldRoot = rootsByDegree(degree)
        mergedRoot = if (mergedRoot.priority < oldRoot.priority) {
          setParent(oldRoot, mergedRoot)
        } else {
          setParent(mergedRoot, oldRoot)
        }
        rootsByDegree.remove(degree)
        degree += 1
      }
      rootsByDegree(mergedRoot.degree) = mergedRoot
      minRoot = entryOrd.min(mergedRoot, minRoot)
    }
    minRoot
  }
}

object FibonacciHeap {
  class Entry[V, P](val value: V, var priority: Option[P]) {
    private[FibonacciHeap] var next: Entry[V, P] = this
    private[FibonacciHeap] var previous: Entry[V, P] = this
    private[FibonacciHeap] var oParent: Option[Entry[V, P]] = None
    private[FibonacciHeap] var oFirstChild: Option[Entry[V, P]] = None
    private[FibonacciHeap] var degree = 0
    private[FibonacciHeap] var isMarked = false

    private[FibonacciHeap] def children: List[Entry[V, P]] = getCycle(oFirstChild)
  }
  object Entry {
    implicit def unwrapEntry[V, P](entry: Entry[V, P]): V = entry.value
    implicit def ordering[V, P](implicit ord: Ordering[P]): Ordering[Entry[V, P]] = Ordering.by(_.priority)
  }

  private[FibonacciHeap] def getCycle[V,P](oStart: Option[Entry[V, P]]): List[Entry[V, P]] = {
    oStart match {
      case None => Nil
      case Some(start) => start :: Iterator.iterate(start)(_.next).drop(1).takeWhile(_ != start).toList
    }
  }

  class DepthFirstEntryIterator[V, P](root: Entry[V, P]) extends AbstractIterator[Entry[V, P]] {
    private var current: Entry[V, P] = root
    private var path: List[Entry[V, P]] = List(root)
    private var _hasNext: Boolean = true

    override def hasNext: Boolean = _hasNext

    override def next(): Entry[V, P] = {
      if (!_hasNext) throw new NoSuchElementException("next on empty iterator")
      current.oFirstChild match {
        case Some(c) =>
          val old = current
          current = c
          path = c :: path
          old
        case None =>
          val old = current
          while (current.next == path.head && current.oParent.isDefined) {
            current = current.oParent.get
            path = path.tail
          }
          if (current.next == path.head) {
            _hasNext = false
          } else {
            current = current.next
          }
          old
      }
    }
  }
  
  /**
   * Joins two Fibonacci heaps into a new one. No heap consolidation is
   * performed; the two root lists are just spliced together.
   * Runtime: O(1)
   */
  def merge[V, P](a: FibonacciHeap[V, P], b: FibonacciHeap[V, P]): FibonacciHeap[V, P] = {
    require(a.ord == b.ord, "Heaps that use different orderings can't be merged.")
    val result = new FibonacciHeap[V, P]()(a.ord)
    result.oMinEntry = a.mergeLists(a.oMinEntry, b.oMinEntry)
    result.len = a.size + b.size
    result
  }

  private def unlinkFromNeighbors[V, P](entry: Entry[V, P]) {
    entry.previous.next = entry.next
    entry.next.previous = entry.previous
    entry.previous = entry
    entry.next = entry
  }
}
