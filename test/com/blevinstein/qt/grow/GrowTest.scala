package com.blevinstein.qt.grow

import com.blevinstein.qt.Material
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf,QuadAddr}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class GrowTest extends FunSuite with Matchers {
  test("update - deterministic growth") {
    val rules = List(
      new ReplacementRule(
        new QuadBranch(new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Empty)),
        new QuadLeaf(Material.Full)))

    val q1 = new QuadTree.Builder()
      .add(new QuadAddr(BottomLeft, BottomLeft), Material.Full)
      .build

    val q2 = ReplacementRule.update(rules)(q1)
    val q3 = ReplacementRule.update(rules)(q2)

    q1 shouldEqual new QuadBranch(new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Empty),
      new QuadBranch(new QuadLeaf(Material.Empty),
        new QuadLeaf(Material.Empty),
        new QuadLeaf(Material.Full),
        new QuadLeaf(Material.Empty)),
      new QuadLeaf(Material.Empty))
    q2 shouldEqual new QuadBranch(new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Full),
      new QuadLeaf(Material.Empty))
    q3 shouldEqual new QuadLeaf(Material.Full)
  }

  test("GrowthSim - deterministic growth") {
    val genome = new QuadGenome(List(
      new ReplacementRule(
        new QuadBranch(new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Empty)),
        new QuadLeaf(Material.Full))))

    GrowthSim(genome) shouldEqual
        (new QuadBranch(new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Empty)),
        GrowthSim.environmentSize - 1)
  }
}
