package com.samthomson.utils

import java.io.PrintStream

case class Timer(nanoTime: () => Long, log: PrintStream) {
  def timeAndLog[A](a: => A): A = timeAndLogMsg("")(a)
  def timeAndLogMsg[A](msg: String)(a: => A): A = {
    val (result, elapsedNanos) = time(a)
    val paddedMsg = if (msg.isEmpty) msg else msg + " "
    log.println(f"${paddedMsg}completed in ${elapsedNanos * 1e-6}%,.3f millis")
    result
  }
  def time[A](a: => A): (A, Long) = {
    val start = nanoTime()
    val result = a
    val elapsedNanos = nanoTime() - start
    (result, elapsedNanos)
  }
}
object Timer {
  lazy val default = Timer(System.nanoTime, System.err)
}
