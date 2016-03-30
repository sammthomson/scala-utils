package com.samthomson.utils

import com.samthomson.utils.TraversableOnceOps._


class TakeToOpsTest extends BaseTest {
  behavior of "takeTo"

  it should "return a Range when given a Range" in {
    val result = (1 to 100).takeTo(_ > 6)
    result should be (a [Range])
    result should be (1 to 7)
  }

  it should "return an Iterator when given an Iterator" in {
    val ones = Iterator.continually(1)  // infinite
    val result = ones.takeTo(_ => false)  // still infinite
    result.toString should be ("non-empty iterator") // i.e. previous line should terminate
  }

  it should "match an alternate version using span" in {
    val p: Boolean => Boolean = identity
    forAll { (input: Vector[Boolean]) =>
      val result = input.takeTo(p)
      val expected = {
        val (l, r) = input.span(!p(_))
        l ++ r.take(1)
      }
      result should be (a [Vector[_]])
      result should be (expected)
    }
  }
}
