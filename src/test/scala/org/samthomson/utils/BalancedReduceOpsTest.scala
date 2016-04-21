package org.samthomson.utils

import TraversableOnceOps.BalancedReduceOps
import math._

class BalancedReduceOpsTest extends BaseTest {

  behavior of "balancedReduce"

  it should "handle a large stream" in {
    val len = 1234567
    val input = Iterator.fill(len)(1)
    var numReductions = 0
    var maxOperand = 0
    val result = input.balancedReduce(0)((i, j) => {
      numReductions += 1
      maxOperand = maxOperand max i max j
      i + j
    })
    result should be (len)
    numReductions should be (len - 1)
    maxOperand should be (pow(2, ceil(log(len) / log(2)).toInt - 1).toInt)
  }
}
