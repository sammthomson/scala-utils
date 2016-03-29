package com.samthomson.utils

import com.samthomson.utils.TraversableOnceOps._


class TakeToOpsTest extends BaseTest {
  behavior of "takeTo"

  it should "match a small fixture" in {
    val result = (1 to 10).takeTo(_ > 6)
    result should be (1 to 7)
  }
}
