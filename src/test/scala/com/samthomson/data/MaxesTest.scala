package com.samthomson.data

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}
import TraversableOnceOps._

class MaxesTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "maxes" should "be non-empty for non-empty input" in {
    forAll { (head: Int, tail: List[Int]) =>
      val maxes = (head :: tail).maxes
      maxes shouldNot be (empty)
    }
  }

  it should "be empty for empty lists" in {
    Vector[Int]().maxes should be (empty)
  }

  it should "all be gteq everything in the input" in {
    forAll { (head: (Short, Int), tail: List[(Short, Int)]) =>
      val input = head :: tail
      val maxes = input.maxesBy(_._2)
      for (
        (_, m) <- maxes;
        (_, i) <- input
      ) {
        m shouldBe >= (i)
      }
    }
  }
}
