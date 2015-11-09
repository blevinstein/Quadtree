package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class TransformTest extends FunSuite with Matchers {
  test("Transform") {
    val q1 = new QuadBranch(new QuadLeaf(true),
        new QuadLeaf(false),
        new QuadLeaf(false),
        new QuadBranch(new QuadLeaf(true),
          new QuadLeaf(false),
          new QuadLeaf(false),
          new QuadLeaf(true)))

    Transform.rotateLeft(q1) shouldEqual
        new QuadBranch(new QuadLeaf(false),
          new QuadBranch(new QuadLeaf(false),
            new QuadLeaf(true),
            new QuadLeaf(true),
            new QuadLeaf(false)),
          new QuadLeaf(true),
          new QuadLeaf(false))

    Transform.rotateRight(q1) shouldEqual
        new QuadBranch(new QuadLeaf(false),
          new QuadLeaf(true),
          new QuadBranch(new QuadLeaf(false),
            new QuadLeaf(true),
            new QuadLeaf(true),
            new QuadLeaf(false)),
          new QuadLeaf(false))

    Transform.mirror(q1) shouldEqual
        new QuadBranch(new QuadLeaf(false),
          new QuadLeaf(true),
          new QuadBranch(new QuadLeaf(false),
            new QuadLeaf(true),
            new QuadLeaf(true),
            new QuadLeaf(false)),
          new QuadLeaf(false))
  }
}
