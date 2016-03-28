package com.samthomson.data.mutable

import com.samthomson.data.TraversableOnceOps.MaxesOps

import scala.annotation.tailrec
import scala.collection.{mutable => m}


class UfNode[V](val data: V,
                var parent: Option[UfNode[V]],
                var rank: Int) {
  /**
   * Finds the representative for the given item.
   * Flattens the `parents` tree as it traverses up it.
   */
  def component: UfNode[V] = {
    var pathToRepresentative = List[UfNode[V]]()
    var current = this
    while (current.parent.isDefined) {
      pathToRepresentative = current :: pathToRepresentative
      current = current.parent.get
    }
    // flatten
    for (node <- pathToRepresentative) {
      node.parent = Some(current)
    }
    current
  }

  def sameComponent(other: UfNode[V]): Boolean = component == other.component

  def merge(other: UfNode[V]): UfNode[V] = {
    val components = Vector(this, other).map(_.component)
    if (components.head == components.last) {
      // already the same component
      components.head
    } else {
      // try to keep balanced by putting the shorter tree under the taller one
      val Seq(shorter, taller) = components.sortBy(_.rank)
      shorter.parent = Some(taller)
      if (taller.rank == shorter.rank) {
        // tallest tree just got taller
        taller.rank += 1
      }
      taller
    }
  }
}
object UfNode {
  def apply[V](data: V): UfNode[V] = new UfNode[V](data, None, 0)
}



/** A mutable Union-Find data structure */
class UnionFind[V](private val parents: m.Map[V, V], private val rank: m.Map[V, Int]) {
  /**
   * Finds the representative for the given item.
   * Flattens the `parents` tree as it traverses up it.
   */
  def componentOf(x: V): V = componentOfHelper(x, Nil)

  @tailrec
  private def componentOfHelper(x: V, descendants: List[V]): V = {
    val directParent = parents(x)
    if (directParent == x) {
      // we found the top. flatten this branch of the tree
      for (d <- descendants) { parents(d) = x }
      x
    } else {
      componentOfHelper(directParent, x :: descendants)
    }
  }

  /** Merges the given components and returns the representative of the new component */
  def merge(xs: V*): Option[V] = {
    val components = xs.map(componentOf).toSet
    if (components.size < 2) return components.headOption // already all the same component
    // try to keep balanced by putting shorter trees under the tallest one
    val tallestComponents = components.maxesBy(rank)
    val tallestComponent = tallestComponents.head
    components foreach { h => parents(h) = tallestComponent }
    if (tallestComponents.size > 1) {
      // tallest tree just got taller
      rank(tallestComponent) += 1
    }
    Some(tallestComponent)
  }

  /** Determines whether the two items are in the same component or not */
  def sameComponent(a: V, b: V): Boolean = componentOf(a) == componentOf(b)
}

object UnionFind {
  /** Constructs a new partition of singletons */
  def singletons[T]: UnionFind[T] = {
    new UnionFind[T](parents = m.Map().withDefault(identity), rank = m.Map().withDefaultValue(0))
  }
}
