package com.samthomson.data

import TraversableOnceOps._


class MaxesTest extends BaseTest {
  def f(x: (Short, Int)): Int = x._2

  "maxes" should "be non-empty for non-empty input" in {
    forAll { (head: Int, tail: List[Int]) =>
      val input = head :: tail
      input.maxes shouldNot be (empty)
      input.mins shouldNot be (empty)
    }
  }

  it should "be empty for empty lists" in {
    val input = Vector[Int]()
    input.maxes should be (empty)
    input.mins should be (empty)
  }

  it should "all be gteq everything in the input" in {
    forAll { (input: List[(Short, Int)]) =>
      for (m <- input.maxesBy(f); i <- input) {
        f(m) shouldBe >= (f(i))
      }
      for (m <- input.minsBy(f); i <- input) {
        f(m) shouldBe <= (f(i))
      }
    }
  }

  it should "all be contained in the input" in {
    forAll { (input: List[(Short, Int)]) =>
      for (m <- input.maxesBy(f)) {
        input should contain (m)
      }
      for (m <- input.minsBy(f)) {
        input should contain (m)
      }
    }
  }
}
