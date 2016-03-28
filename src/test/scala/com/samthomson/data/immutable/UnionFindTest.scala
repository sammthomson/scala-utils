package com.samthomson.data.immutable

import com.samthomson.data.BaseTest
import com.samthomson.data.immutable.UnionFind.{singletons, merge, sameComponent, connectedComponents}
import scalaz._
import Scalaz._


class UnionFindTest extends BaseTest {
  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 10, maxSize = 20)

  "UnionFind.singletons" should "have no same components at first" in {
    forAll { xs: Set[Int] =>
      val expectedComponents = xs.toSeq map (Seq(_))
      singletons[Int] |> shouldHaveComponents (expectedComponents)
    }
  }

  "A UnionFind" should "have expected components after merging" in {
    forAll { (rawComponents: List[Seq[Int]]) =>
      // zip elements with component idx to ensure components are disjoint
      val components = rawComponents.zipWithIndex map { case (c, i) => c map ((_, i)) }
      val initial = singletons[(Int, Int)]
      // merge everything within a component
      val allAtOnceMerged = initial |> (components traverseS merge).exec
      allAtOnceMerged |> shouldHaveComponents (components)

      // make sure it also works when merging 2 at a time
      val withinComponentPairs = components flatMap (_.sliding(2))
      val pairwiseMerged = initial |> (withinComponentPairs traverseS merge).exec
      pairwiseMerged |> shouldHaveComponents (components)
    }
  }

  def shouldHaveComponents[T](expectedComponents: Seq[Seq[T]])(unionFind: UnionFind[T]) {
    // everything within the same expectedComponent should be in the same component
    for (
      expectedComponent <- expectedComponents;
      Seq(x, y) <- expectedComponent.combinations(2)
    ) {
      unionFind |> sameComponent(x, y).eval shouldBe true
    }
    // anything not in the same expectedComponents should not be in the same component
    for (
      List(componentA, componentB) <- expectedComponents.combinations(2);
      a <- componentA;
      b <- componentB
    ) {
      unionFind |> sameComponent(a, b).eval shouldBe false
    }
    // `connectedComponents` should match `expectedComponents`
    val allItems = expectedComponents.flatten
    val components = unionFind |> connectedComponents(allItems).eval
    components shouldBe (expectedComponents map (_.toSet) filter (_.nonEmpty)).toSet
  }
}
