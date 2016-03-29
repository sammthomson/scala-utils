package com.samthomson.utils

class DefaultDictTest extends BaseTest {
  behavior of "DefaultDict"

  it should "work as a counter" in {
    forAll { (xs: List[Int]) =>
      val dd = DefaultDict[Int, Int](0)
      xs foreach { dd(_) += 1 }
      xs foreach { x =>
        dd(x) should be (xs.count(_ == x))
      }
    }
  }
}
