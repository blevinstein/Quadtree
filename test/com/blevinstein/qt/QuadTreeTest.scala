package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class QuadTreeTest extends FunSuite with Matchers {
  test("QuadTree.Builder") {
    (new QuadTree.Builder().build shouldEqual new QuadLeaf(Material.Empty))

    (new QuadTree.Builder().add(List(TopLeft), Material.Full).build
      shouldEqual
        new QuadBranch(new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Empty)))

    (new QuadTree.Builder()
        .add(List(TopLeft), Material.Full)
        .add(List(TopRight, TopLeft), Material.Full)
        .build
      shouldEqual
        new QuadBranch(
          new QuadLeaf(Material.Full),
          new QuadBranch(
            new QuadLeaf(Material.Full),
            new QuadLeaf(Material.Empty),
            new QuadLeaf(Material.Empty),
            new QuadLeaf(Material.Empty)),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Empty)))
  }
}
