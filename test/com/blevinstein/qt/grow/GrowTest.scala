package com.blevinstein.qt.grow

import com.blevinstein.qt.Material
import com.blevinstein.qt.Material.{Empty,Full}
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf,QuadAddr}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class GrowTest extends FunSuite with Matchers {
  test("update - deterministic growth") {
    val rules = List(
      // expand from bottom left corner into empty space
      new ReplacementRule(
        new QuadBranch(new QuadLeaf(ChangeMaterial(Empty, Full)),
          new QuadLeaf(ChangeMaterial(Empty, Full)),
          new QuadLeaf(MatchMaterial(Full)),
          new QuadLeaf(ChangeMaterial(Empty, Full)))))

    val q1 = new QuadTree.Builder[Material](Empty)
      .add(new QuadAddr(BottomLeft, BottomLeft), Full)
      .build

    val q2 = ReplacementRule.update(rules)(q1)
    val q3 = ReplacementRule.update(rules)(q2)

    q1 shouldEqual new QuadBranch(new QuadLeaf(Empty),
      new QuadLeaf(Empty),
      new QuadBranch(new QuadLeaf(Empty),
        new QuadLeaf(Empty),
        new QuadLeaf(Full),
        new QuadLeaf(Empty)),
      new QuadLeaf(Empty))
    q2 shouldEqual new QuadBranch(new QuadLeaf(Empty),
      new QuadLeaf(Empty),
      new QuadLeaf(Full),
      new QuadLeaf(Empty))
    q3 shouldEqual new QuadLeaf(Full)
  }

  test("GrowthSim - deterministic growth") {
    val genome = new QuadGenome(List(
      // expand from top right corner into empty space
      new ReplacementRule(
        new QuadBranch(new QuadLeaf(ChangeMaterial(Empty, Full)),
          new QuadLeaf(MatchMaterial(Full)),
          new QuadLeaf(ChangeMaterial(Empty, Full)),
          new QuadLeaf(ChangeMaterial(Empty, Full))))))

    GrowthSim(genome) shouldEqual
        (new QuadBranch(new QuadLeaf(Empty),
          new QuadLeaf(Empty),
          new QuadLeaf(Full),
          new QuadLeaf(Empty)),
        GrowthSim.environmentSize - 1)
  }
}
