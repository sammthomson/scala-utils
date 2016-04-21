package org.samthomson.utils

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.Iterator.iterate

object UnfoldTest {
  type State = (Boolean, Boolean) // small state space (4 states)
  type Emit = Boolean
  type Step = State => Option[(Emit, State)]

  val MaxLen = 10 // so we don't iterate forever

  implicit def stepArb: Arbitrary[Step] = Arbitrary[Step](
    arbitrary[Map[State, (Emit, State)]].map(_.get)
  )
}
class UnfoldTest extends BaseTest {
  import UnfoldTest._
  import AnyOps._

  behavior of "unfold"

  it should "match a small fixture" in {
    def step(i: Int): Option[(Char, Int)] = if (i >= 9) None else Some((('a' + i + 1).toChar, i + 1))
    val result = 0 unfold step
    val expected = 'b' to 'j'
    result.toList should be (expected.toList)
  }

  it should "match Iterator.iterate" in {
    forAll { (start: State, step: Step) =>
      val result = start unfold step
      val expected =
        iterate(Option((null.asInstanceOf[Emit], start))){ _.map(_._2).flatMap(step) }
            .drop(1)
            .takeWhile(_.isDefined)
            .map(_.get)
            .map(_._1)
      result.take(MaxLen).toList should be (expected.take(MaxLen).toList)
    }
  }

  behavior of "iterate"

  it should "match a small fixture" in {
    def step(c: Char): Option[Char] = if (c >= 'j') None else Some((c + 1).toChar)
    val result = 'a' iterate step
    val expected = 'a' to 'j'
    result.toList should be (expected)
  }

  it should "match Iterator.iterate" in {
    forAll { (start: State, st: Step) =>
      def step(s: State): Option[State] = st(s).map(_._2)
      val result = start iterate step
      val expected =
        iterate(Option(start))(_.flatMap(step))
            .takeWhile(_.isDefined)
            .map(_.get)
      result.take(MaxLen).toList should be (expected.take(MaxLen).toList)
    }
  }
}
