package com.samthomson.utils

import java.io.PrintStream
import com.sun.xml.internal.ws.util.ByteArrayBuffer


class TimerTest extends BaseTest {

  behavior of "time"

  it should "time" in {
    val mockClock = Iterator.from(0, 1234000)
    val mockLog = new ByteArrayBuffer()
    val timer = Timer(mockClock.next, new PrintStream(mockLog))
    val (result, time) = timer.time { Some(true) }
    result should be (Some(true))
    time should be (1234000)
    mockLog.toString should be ("")
  }

  behavior of "timeAndLogMsg"

  it should "time and log msg" in {
    val mockClock = Iterator.from(0, 1234000)
    val mockLog = new ByteArrayBuffer()
    val timer = Timer(mockClock.next, new PrintStream(mockLog))
    val result = timer.timeAndLogMsg("asdf") { Some(true) }
    result should be (Some(true))
    mockLog.toString should be ("asdf completed in 1.234 millis\n")
  }
}
