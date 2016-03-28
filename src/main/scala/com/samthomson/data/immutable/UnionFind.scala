package com.samthomson.data.immutable

import com.samthomson.data.state._
import com.samthomson.data.TraversableOnceOps._
import com.samthomson.data.immutable.MapWithDefaultLens._

import monocle.macros.Lenses
import scala.collection.Iterator.iterate
import scalaz._, Scalaz._


/**
 * An immutable Union-Find data structure.
 * Because every method potentially flattens the tree as a side-effect,
 * we also return the new (possibly flatter) UnionFind.
 * Scalaz's State makes this slightly less annoying.
 * `rank(x)` is an upper bound on the height of `x`.
 */
@Lenses("_")
case class UnionFind[T] private[UnionFind] (parents: Map[T, T], ranks: Map.WithDefault[T, Int])

object UnionFind {
  /** Constructs a new UnionFind of singletons */
  def singletons[T]: UnionFind[T] =
    UnionFind[T](parents = Map(), ranks = new Map.WithDefault(Map(), _ => 0))

  /** Finds the representative for `x`. Flattens all ancestors of `x`. */
  def componentOf[T](x: T): State[UnionFind[T], T] =
    for (
      parents <- _parents[T].gets;
      // find the representative for `x`, remembering the path up so that it can be flattened.
      rep :: path = (iterate(x)(parents) takeTo (!parents.contains(_))).toList.reverse;
      // for every element in `path`, set parent to `rep`
      _ <- _parents[T] mods (_ ++ (path map (_ -> rep)))
    ) yield rep

  /** Merges the given components and returns the representative of the new component. */
  def merge[T](xs: Seq[T]): State[UnionFind[T], Option[T]] =
    if (xs.size < 2) {
      state(xs.headOption)  // nothing to merge
    } else {
      for (
        components <- xs.toList.traverseS(componentOf).map(_.toSet);
        // Put all components under one new representative.
        // Try to keep balanced by putting shorter trees under a tallest one
        ranks <- _ranks[T].gets;
        tallestComponents = components.maxesBy(ranks);
        newRep = tallestComponents.head;
        _ <- _parents[T] mods {
          _ ++ ((components - newRep) map (_ -> newRep))
        };
        // if there's a tie, newRep just got taller
        heightChange = if (tallestComponents.size > 1) 1 else 0;
        _ <- _ranks[T] ^|-> at(newRep) mods (_ + heightChange)
      ) yield some(newRep)
    }

  /** Determines whether the two items are in the same component or not */
  def sameComponent[T](a: T, b: T): State[UnionFind[T], Boolean] =
    for (ac <- componentOf(a); bc <- componentOf(b)) yield ac == bc

  /** Returns the set of connected components */
  def connectedComponents[T](xs: Seq[T]): State[UnionFind[T], Set[Set[T]]] =
    for (components <- xs.toList.traverseS(componentOf)) yield
      (components zip xs).toMultimap[Set].values.toSet
}
