package com.samthomson.utils.data

import scala.collection.{mutable => m}

/**
 * Like Python's `defaultdict`.
 * Updates the underlying Map with the default (`d`) on any key miss.
 */
class DefaultDict[K, V](underlying: m.HashMap[K, V], d: K => V)
  extends m.Map.WithDefault[K, V](underlying, k => { val v = d(k); underlying += k -> v; v })

object DefaultDict {
  def apply[K, V](d: K => V): DefaultDict[K, V] = new DefaultDict(m.HashMap[K, V](), d)
  def apply[K, V](d: V): DefaultDict[K, V] = new DefaultDict(m.HashMap[K, V](), _ => d)
}
