package org.samthomson.utils

class TimerTest extends BaseTest {

  behavior of "time"

  it should "time" in {
    val mockClock = Iterator.from(0, 1234000)
    val mockLog = StringBuilder.newBuilder
    val timer = Timer(mockClock.next, mockLog ++= _ + "\n")
    val (result, time) = timer.time { Some(true) }
    result should be (Some(true))
    time should be (1234000)
    mockLog.toString should be ("")
  }

  behavior of "timeAndLogMsg"

  it should "time and log msg" in {
    val mockClock = Iterator.from(0, 1234000)
    val mockLog = StringBuilder.newBuilder
    val timer = Timer(mockClock.next, mockLog ++= _ + "\n")
    val result = timer.timeAndLogMsg("asdf") { Some(true) }
    result should be (Some(true))
    mockLog.toString should be ("asdf completed in 1.234 millis\n")
  }
}
