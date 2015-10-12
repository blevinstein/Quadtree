package com.blevinstein.qt.grow

import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf,QuadAddr}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class GrowTest extends FunSuite with Matchers {
  test("update - deterministic growth") {
    val rules = List(
      // expand from bottom left corner into empty space
      new ReplacementRule(
        new QuadBranch(new QuadLeaf(ChangeMaterial(false, true)),
          new QuadLeaf(ChangeMaterial(false, true)),
          new QuadLeaf(MatchMaterial(true)),
          new QuadLeaf(ChangeMaterial(false, true)))))

    val q1 = new QuadTree.Builder[Boolean](false)
      .add(new QuadAddr(BottomLeft, BottomLeft), true)
      .build

    val q2 = ReplacementRule.update(rules)(q1)
    val q3 = ReplacementRule.update(rules)(q2)

    q1 shouldEqual new QuadBranch(new QuadLeaf(false),
      new QuadLeaf(false),
      new QuadBranch(new QuadLeaf(false),
        new QuadLeaf(false),
        new QuadLeaf(true),
        new QuadLeaf(false)),
      new QuadLeaf(false))
    q2 shouldEqual new QuadBranch(new QuadLeaf(false),
      new QuadLeaf(false),
      new QuadLeaf(true),
      new QuadLeaf(false))
    q3 shouldEqual new QuadLeaf(true)
  }

  test("GrowthSim - deterministic growth") {
    val genome = new QuadGenome(List(
      // expand from top right corner into empty space
      new ReplacementRule(
        new QuadBranch(new QuadLeaf(ChangeMaterial(false, true)),
          new QuadLeaf(MatchMaterial(true)),
          new QuadLeaf(ChangeMaterial(false, true)),
          new QuadLeaf(ChangeMaterial(false, true))))))

    GrowthSim(genome) shouldEqual
        (new QuadBranch(new QuadLeaf(false),
          new QuadLeaf(false),
          new QuadLeaf(true),
          new QuadLeaf(false)),
        GrowthSim.environmentSize - 1)
  }
}
