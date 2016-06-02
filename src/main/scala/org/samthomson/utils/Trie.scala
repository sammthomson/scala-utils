package org.samthomson.utils

import scala.Predef.{any2stringadd => _, _}
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.SortedMap
import scala.collection.{mutable => m, AbstractIterator, MapLike}


/**
  * An immutable map from [[CharSequence]]s to [[B]]s, optimized for querying prefixes.
  *
  * @param root  the value associated with [[""]]
  * @param suffixes map from head -> [[Trie]] of tails.
  * @tparam B the type of values.
  */
case class Trie[+B] private (root: Option[B],
                             suffixes: SortedMap[Char, Trie[B]])
    extends Map[CharSequence, B]
        with MapLike[CharSequence, B, Trie[B]] {

  import Trie.unroll

  @tailrec
  final override def get(key: CharSequence): Option[B] = unroll(key) match {
    case None => root
    case Some((hd, tl)) => suffixes.get(hd) match {
      case None => None
      case Some(child) => child.get(tl)
    }
  }

  override def updated[B1 >: B](k: CharSequence, v: B1): Trie[B1] = this + ((k, v))

  override def +[B1 >: B](kv: (CharSequence, B1)): Trie[B1] = {
    val (k, v) = kv
    unroll(k) match {
      case None => this.copy(root = Some(v))  // k == "", update root
      case Some((hd, tl)) =>  // add tl to appropriate child, creating new child if necessary
        val child = suffixes.getOrElse(hd, Trie.empty)
        this.copy(suffixes = suffixes.updated(hd, child.updated(tl, v)))
    }
  }

  override def -(key: CharSequence): Trie[B] = unroll(key) match {
    case None => this.copy(root = None)  // k == "", update root
    case Some((hd, tl)) => suffixes.get(hd) match {
      case None => this  // nothing to remove
      case Some(child) => this.copy(suffixes = suffixes.updated(hd, child - tl)) // remove tl from appropriate child
    }
  }

  /** Equivalent to (but more efficient than) filtering out keys that don't start with `prefix`,
    * then removing `prefix` from the beginning of each key. */
  @tailrec
  final def withPrefix(prefix: CharSequence): Trie[B] = unroll(prefix) match {
    case None => this
    case Some((hd, tl)) => suffixes.get(hd) match {
      case None => empty
      case Some(child) => child.withPrefix(tl)
    }
  }

  override def iterator: Iterator[(String, B)] = new Trie.TrieIterator[B](this)

  override def isEmpty: Boolean = root.isEmpty && suffixes.isEmpty

  override lazy val size: Int = root.size + suffixes.valuesIterator.map(_.size).sum

  override def toString: String = {
    val rootStr = root.map("\"\" -> " + _).toList
    val childStrs = suffixes.map({ case (c, v) => s"$c -> $v" }).toList
    "{" + (rootStr ++ childStrs).mkString(", ") + "}"
  }

  // scala.collections boilerplate

  override def empty: Trie[B] = Trie.empty
}

object Trie {
  val empty: Trie[Nothing] = new Trie(None, SortedMap.empty)

  def apply[B](elems: TraversableOnce[(CharSequence, B)]): Trie[B] = (newBuilder[B] ++= elems).result()

  def apply[B](elems: (CharSequence, B)*): Trie[B] = apply(elems)

  /** If input starts with any of the given strings in `prefixes`,
    * returns the matching prefix and the associated value, or None if not found. */
  def matchAny[B](prefixes: Trie[B], input: CharSequence): Option[(CharSequence, Any)] = matchAny(prefixes, input, 0)

  /** If input has any of the given strings in `prefixes` starting at `startIndex`,
    * returns the matching prefix and the associated value, or None if not found. */
  def matchAny[B](prefixes: Trie[B], input: CharSequence, startIndex: Int): Option[(CharSequence, Any)] = {
    @tailrec
    def go(i: Int, trie: Trie[B]): Option[(CharSequence, B)] =
      trie.root match {
        case Some(v) => Some((input.subSequence(startIndex, i), v))  // empty prefix matches anything
        case None =>
          if (i >= input.length) None  // ran out of input before anything matched
          else trie.suffixes.get(input.charAt(i)) match {
            case None => None  // no prefix matched
            case Some(child) => go(i + 1, child)  // still possible, keep looking
          }
      }
    go(startIndex, prefixes)
  }

  /** Depth-first (hence alphabetical) iteration through entries in `start` */
  final class TrieIterator[B](start: Trie[B]) extends AbstractIterator[(String, B)] {
    var queue = if (start.isEmpty) Nil else List((Vector[Char](), start))  // invariant: no empty tries in queue

    override def hasNext: Boolean = queue.nonEmpty

    @tailrec
    override def next(): (String, B) = queue match {
      case Nil => Iterator.empty.next()
      case (prefix, trie) :: remaining =>
        queue = trie.suffixes.toList.map({ case (c, child) => (prefix :+ c, child) }) ::: remaining
        trie.root match {
          case None => next()
          case Some(v) => (prefix.mkString(""), v)
        }
    }
  }

  /** Splits s into its head and tail if s is non-empty */
  private def unroll(s: CharSequence): Option[(Char, CharSequence)] = {
    val len = s.length
    if (len == 0) None else Some((s.charAt(0), s.subSequence(1, len)))
  }

  // scala.collections boilerplate

  def newBuilder[B]: m.Builder[(CharSequence, B), Trie[B]] =
    new m.Builder[(CharSequence, B), Trie[B]] {
      private var elems: Trie[B] = empty
      def +=(x: (CharSequence, B)): this.type = { elems += x; this }
      def clear() { elems = empty }
      def result: Trie[B] = elems
    }

  implicit def canBuildFrom[B]: CanBuildFrom[Trie[_], (CharSequence, B), Trie[B]] =
    new CanBuildFrom[Trie[_], (CharSequence, B), Trie[B]] {
      def apply(from: Trie[_]): m.Builder[(CharSequence, B), Trie[B]] = newBuilder
      def apply(): m.Builder[(CharSequence, B), Trie[B]] = newBuilder
    }
}
