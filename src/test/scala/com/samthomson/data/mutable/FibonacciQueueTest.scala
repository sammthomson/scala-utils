package com.samthomson.data.mutable

import com.samthomson.data.BaseTest

import scala.collection.immutable.Seq
import scala.util.Random

class FibonacciQueueTest extends BaseTest {
  "A FibonacciQueue"  should "match SeqLike.sorted" in {
    forAll { input: Vector[Float] =>
      val result = {
        val q: FibonacciQueue[Float] = new FibonacciQueue[Float]()(Ordering[Float].reverse) ++ input
        Iterator.continually(q.dequeue()).take(input.size)
      }
      result.toVector should equal (input.sorted)
    }
  }

  it should "sort a long list" in {
    // ScalaCheck won't generate large Seqs by default
    val input: Seq[Int] = Random.shuffle((0 until 100000).toVector)
    val result = {
      val q: FibonacciQueue[Int] = new FibonacciQueue[Int]()(Ordering[Int].reverse) ++ input
      Iterator.continually(q.dequeue()).take(input.size).toVector
    }
    result should equal (input.sorted)
  }
}
