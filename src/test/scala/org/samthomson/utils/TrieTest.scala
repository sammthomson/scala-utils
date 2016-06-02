package org.samthomson.utils

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._


object TrieTest {
  implicit def trieArb[B : Arbitrary]: Arbitrary[Trie[B]] = Arbitrary[Trie[B]](
    arbitrary[Seq[(String, B)]].map(Trie(_))
  )
}
class TrieTest extends BaseTest {
  import TrieTest._

  behavior of "Trie.empty"

  it should "not contain any entries" in {
    forAll { (k: String) =>
      Trie.empty.contains(k) should equal (false)
      Trie.empty.get(k) should equal (None)
    }
  }

  behavior of "Trie.+"

  it should "add the entry" in {
    forAll { (trie: Trie[Int], k: String, v: Int) =>
      val updated = trie + ((k, v))
      updated.contains(k) should equal (true)
      updated.get(k) should equal (Some(v))
    }
  }

  it should "be idempotent" in {
    forAll { (trie: Trie[Int], k: String, v: Int) =>
      val once = trie + ((k, v))
      val twice = once + ((k, v))
      twice should equal (once)
    }
  }

  behavior of "Trie.-"

  it should "remove the entry" in {
    forAll { (trie: Trie[Int], k: String, v: Int) =>
      (trie - k).contains(k) should equal (false)
      (trie - k).get(k) should equal (None)
      val updated = trie + ((k, v))
      (updated - k).contains(k) should equal (false)
      (updated - k).get(k) should equal (None)
    }
  }

  it should "be idempotent" in {
    forAll { (trie: Trie[Int], k: String, v: Int) =>
      val once = trie - k
      val twice = once - k
      twice should equal (once)
    }
  }

  behavior of "Trie.withPrefix"

  it should "filter out keys that don't start with prefix, and remove prefix from start of each key" in {
    forAll { (trie: Trie[Int], prefix: String) =>
      // naive implementation
      val expected = trie.collect { case (k, v) if k.toString.startsWith(prefix) =>
        (k.subSequence(prefix.length, k.length), v)
      }
      trie.withPrefix(prefix) should equal (expected)
    }
  }

  behavior of "Trie.iterator"

  it should "contain each entry" in {
    forAll { (m: Map[String, Int]) =>
      val trie = Trie(m)
      trie.iterator.toSet should equal (m.toSet)
      for ((k, v) <- trie.iterator) {
        m.get(k) should equal (Some(v))
        trie.get(k) should equal (Some(v))
      }
    }
  }

  it should "be in order" in {
    forAll { (trie: Trie[Int]) =>
      val entries = trie.iterator.toVector
      entries.sortBy(_._1) should equal (entries)
    }
  }

  behavior of "Trie.matchAny"

  it should "work for small fixtures" in {
    val prefixes = Trie(
      "asdf" -> List(1),
      "asd"  -> List(2),
      "bsd"  -> List(3),
      "as"   -> List(4)
    )
    val input = "gibberish asdf more gibberish"
    Trie.matchAny(prefixes, input) should equal (None)
    Trie.matchAny(prefixes, input, 10) should equal (Some(("as", List(4))))
    Trie.matchAny(prefixes, "bsdf") should equal (Some(("bsd", List(3))))
  }
}
