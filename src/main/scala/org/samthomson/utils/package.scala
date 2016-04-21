package org.samthomson

package object utils {
  def timeIt[A](a: => A): A = Timer.default.timeAndLog(a)
}
