package com.samthomson.data.immutable

import IsCovariantSet._
import CovariantSet._
import com.samthomson.data.BaseTest


class CovariantSetTest extends BaseTest {

  "A CovariantSet" should "store only and all distinct values" in {
    forAll { input: Vector[Int] =>
      val result = CovariantSet(input: _*)
      for (v <- input) {
        result.contains(v) should be (true)
      }
      result.toSeq.sorted should equal (input.distinct.sorted)
    }
  }

  it should "be empty when empty" in {
    CovariantSet().isEmpty should be (true)
  }
}
