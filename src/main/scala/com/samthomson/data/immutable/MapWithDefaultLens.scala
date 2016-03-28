package com.samthomson.data.immutable

import monocle.Lens

import scala.collection.immutable.Map.WithDefault
import scala.language.implicitConversions

object MapWithDefaultLens {
  // static version with curried arguments
  def updated[K, V](k: K)(v: V)(m: WithDefault[K, V]): WithDefault[K, V] = m.updated(k, v)

  implicit def at[K, V](k: K): Lens[WithDefault[K, V], V] = {
    Lens[WithDefault[K, V], V](get = _(k))(set = updated(k))
  }
}
