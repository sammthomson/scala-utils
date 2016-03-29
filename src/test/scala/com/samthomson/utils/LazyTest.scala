package com.samthomson.utils

class LazyTest extends BaseTest {
  behavior of "Lazy"

  it should "evaluate exactly once but not until requested" in {
    var count = 0
    val result = Lazy { count += 1; "asdf" }
    count should be (0)
    result.toString should be ("Lazy(?)")
    count should be (0)
    result.force should be ("asdf")
    count should be (1)
    result.force should be ("asdf")
    count should be (1)
    result.toString should be ("Lazy(asdf)")
    count should be (1)
  }
}
