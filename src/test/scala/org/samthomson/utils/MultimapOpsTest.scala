package org.samthomson.utils

class MultimapOpsTest extends BaseTest {
  import TraversableOnceOps._

  val input = List(
    "a" -> 0, "a" -> 1, "a" -> 2,
    "b" -> 3, "b" -> 4,
    "c" -> 5, "c" -> 5
  )

  behavior of "toMultimap"

  it should "match a small fixture" in {
    val result = input.toMultimap
    result should be (Map(
      "a" -> List(0, 1, 2),
      "b" -> List(3, 4),
      "c" -> List(5, 5)))
  }

  it should "be polymorphic in the collection type" in {
    val result = input.toMultimap[Set]
    result should be (Map(
      "a" -> Set(0, 1, 2),
      "b" -> Set(3, 4),
      "c" -> Set(5)))
  }
}
