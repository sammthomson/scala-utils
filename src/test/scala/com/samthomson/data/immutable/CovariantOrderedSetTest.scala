package com.samthomson.data.immutable

import com.samthomson.data.BaseTest


class CovariantOrderedSetTest extends BaseTest {
  import CovariantOrderedSet._
  import CovariantTreeSet._

  "A CovariantSet" should "store only and all distinct values" in {
    forAll { input: Vector[Int] =>
      val result = CovariantTreeSet(input: _*)
      for (v <- input) {
        result.contains(v) should be (true)
      }
      result.toSeq.sorted should equal (input.distinct.sorted)
    }
  }

  it should "be empty when empty" in {
    CovariantTreeSet().isEmpty should be (true)
  }
}
