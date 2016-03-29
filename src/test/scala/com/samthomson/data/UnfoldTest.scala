package com.samthomson.data

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

  behavior of "unfold"

  it should "match a small fixture" in {
    def step(i: Int): Option[(Char, Int)] = if (i >= 9) None else Some((('a' + i + 1).toChar, i + 1))
    val result = unfold(0)(step)
    val expected = 'b' to 'j'
    result.toList should be (expected.toList)
  }

  it should "match Iterator.iterate" in {
    forAll { (start: State, step: Step) =>
      val result = unfold(start)(step)
      val expected =
        iterate(Option((null.asInstanceOf[Emit], start))){ _.map(_._2).flatMap(step) }
            .drop(1)
            .takeWhile(_.isDefined)
            .map(_.get)
            .map(_._1)
      result.take(MaxLen).toList should be (expected.take(MaxLen).toList)
    }
  }

  behavior of "unfoldStates"

  it should "match a small fixture" in {
    def step(c: Char): Option[Char] = if (c >= 'j') None else Some((c + 1).toChar)
    val result = unfoldStates('a')(step)
    val expected = 'b' to 'j'
    result.toList should be (expected.toList)
  }

  it should "match Iterator.iterate" in {
    forAll { (start: State, st: Step) =>
      def step(s: State): Option[State] = st(s).map(_._2)
      val result = unfoldStates(start)(step)
      val expected =
        iterate(Option(start))(_.flatMap(step))
            .drop(1)
            .takeWhile(_.isDefined)
            .map(_.get)
      result.take(MaxLen).toList should be (expected.take(MaxLen).toList)
    }
  }
}
