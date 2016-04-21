package org.samthomson.utils

import TraversableOnceOps._

class LazyMeanTest extends BaseTest {
  behavior of "mean"

  it should "be sum / size" in {
    forAll { (head: Double, tail: List[Double]) =>
      val input = head :: tail
      val mean = input.mean
      val expectedMean = input.sum / input.size
      whenever (!expectedMean.isInfinite) {
        mean should be(roughlyEqualTo(expectedMean))
      }
    }
  }

  "runningMean" should "be sum / size" in {
    forAll { (head: Double, tail: List[Double]) =>
      val input = head :: tail
      val means = input.runningMean
      for ((mean, i) <- means.zipWithIndex) {
        val expected = input.take(i).sum / math.max(i, 1)
        whenever (!expected.isInfinite) {
          mean should be(roughlyEqualTo(expected))
        }
      }
    }
  }
}
