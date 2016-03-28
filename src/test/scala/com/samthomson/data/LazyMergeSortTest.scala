package com.samthomson.data

import com.samthomson.data.TraversableOnceOps.LazyMergeSortOps

import scala.collection.immutable.Seq
import scala.util.Random


class LazyMergeSortTest extends BaseTest {
  "LazyMergeSort.lazySorted"  should "match SeqLike.sorted" in {
    forAll { input: Vector[Float] =>
      val result = input.lazySorted.toVector
      result should equal (input.sorted)
    }
  }

  it should "sort a long list" in {
    // ScalaCheck won't generate large Seqs by default
    val input: Seq[Int] = Random.shuffle((0 until 100000).toVector)
    val result = input.lazySorted
    for ((f, i) <- result.zipWithIndex) {
      f should equal (i)
    }
  }
}
