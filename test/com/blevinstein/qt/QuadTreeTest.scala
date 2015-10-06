package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class QuadTreeTest extends FunSuite with Matchers {
  test("QuadTree.Builder") {
    (new QuadTree.Builder[Boolean](false).build
      shouldEqual
      new QuadLeaf(false))

    (new QuadTree.Builder[Boolean](false).add(new QuadAddr(TopLeft), true)
      .build
      shouldEqual
        new QuadBranch(new QuadLeaf(true),
          new QuadLeaf(false),
          new QuadLeaf(false),
          new QuadLeaf(false)))

    (new QuadTree.Builder[Boolean](false)
        .add(new QuadAddr(TopLeft), true)
        .add(new QuadAddr(TopRight, TopLeft), true)
        .build
      shouldEqual
        new QuadBranch(
          new QuadLeaf(true),
          new QuadBranch(
            new QuadLeaf(true),
            new QuadLeaf(false),
            new QuadLeaf(false),
            new QuadLeaf(false)),
          new QuadLeaf(false),
          new QuadLeaf(false)))
  }

  test("QuadTree.approx") {
    (QuadTree.approx(0, (_) => true)
      shouldEqual
        new QuadLeaf(true))

    (QuadTree.approx(1, (p) =>
        if(p.x + p.y >= 1) true else false)
      shouldEqual
        new QuadBranch(
          new QuadLeaf(true),
          new QuadLeaf(true),
          new QuadLeaf(false),
          new QuadLeaf(true)))

    (QuadTree.approx(2, (p) =>
        if(p.x + p.y >= 1) true else false)
      shouldEqual
        new QuadBranch(
          new QuadBranch(
            new QuadLeaf(true),
            new QuadLeaf(true),
            new QuadLeaf(false),
            new QuadLeaf(true)),
          new QuadLeaf(true),
          new QuadLeaf(false),
          new QuadBranch(
            new QuadLeaf(true),
            new QuadLeaf(true),
            new QuadLeaf(false),
            new QuadLeaf(true))))

    // tests merging
    (QuadTree.approx(3, (p) =>
        if (p.x > 0.5f) true else false)
      shouldEqual
        new QuadBranch(
          new QuadLeaf(false),
          new QuadLeaf(true),
          new QuadLeaf(false),
          new QuadLeaf(true)))
  }

  test("QuadTree.merge") {
    val andFunc = QuadTree.merge((b1: Boolean, b2: Boolean) => (b1 && b2)) _
    val orFunc = QuadTree.merge((b1: Boolean, b2: Boolean) => (b1 || b2)) _

    val q1 = new QuadBranch(
      new QuadLeaf(false),
      new QuadLeaf(true),
      new QuadLeaf(false),
      new QuadLeaf(true))
    val q2 = new QuadBranch(
      new QuadLeaf(false),
      new QuadLeaf(false),
      new QuadLeaf(true),
      new QuadLeaf(true))
    val q3 = new QuadLeaf(false)
    val q4 = new QuadLeaf(true)

    andFunc(q1, q2) shouldEqual
      new QuadBranch(
        new QuadLeaf(false),
        new QuadLeaf(false),
        new QuadLeaf(false),
        new QuadLeaf(true))
    andFunc(q1, q4) shouldEqual q1
    andFunc(q1, q3) shouldEqual q3

    orFunc(q1, q2) shouldEqual
      new QuadBranch(
        new QuadLeaf(false),
        new QuadLeaf(true),
        new QuadLeaf(true),
        new QuadLeaf(true))
    orFunc(q1, q3) shouldEqual q1
    orFunc(q1, q4) shouldEqual q4
  }

  test("QuadTree.merge and calculate overlapping area") {
    def getArea(tree: QuadTree[Boolean]): Float = {
      var totalArea = 0f
      tree.iter { case (addr, bool) =>
          totalArea += (if (bool) addr.toRectangle.area else 0)
      }
      totalArea
    }

    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(false),
        new QuadLeaf(true), new QuadLeaf(true))
    val q2 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
        new QuadLeaf(false), new QuadLeaf(true))

    val matchFunc = QuadTree.merge((b1: Boolean, b2: Boolean) => (b1 == b2)) _

    getArea(matchFunc(q1, q2)) shouldEqual 0.5f
  }

  test("QuadTree.transform and .reduce to calculate area") {
    val avgFunc = QuadTree.reduce((xs: List[Float]) => {
      require(xs.length == 4)
      xs.reduceLeft(_ + _) / 4
    }) _
    val countFunc = QuadTree.transform((m: Boolean) => m match {
      case true => 1f
      case false => 0f
    }) _

    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
        new QuadLeaf(true), new QuadLeaf(false)),
      new QuadLeaf(false))

    val areaOf = (tree: QuadTree[Boolean]) => avgFunc(countFunc(tree))

    areaOf(q1) shouldEqual 3.0/8
  }

  test("QuadTree#getData") {
    val q1 = new QuadBranch(
      new QuadLeaf(false),
      new QuadLeaf(true),
      new QuadLeaf(false),
      new QuadLeaf(true))

    // getData by Point
    q1.getData(new Point(0.1f, 0.1f)) shouldEqual false
    q1.getData(new Point(0.9f, 0.1f)) shouldEqual true

    // getData by address
    q1.getData(new QuadAddr(BottomLeft)) shouldEqual false
    q1.getData(new QuadAddr(BottomRight)) shouldEqual true
    q1.getData(new QuadAddr(BottomRight, BottomLeft)) shouldEqual true
  }

  test("QuadOffset#simplify") {
    // explicit simplify
    new QuadOffset(4, 8, 6).simplify shouldEqual new QuadOffset(3, 4, 3)

    // equals should understand simplification
    assert(new QuadOffset(4, 8, 6) isEqualTo new QuadOffset(3, 4, 3))
    assert(new QuadOffset(3, 4, 3) isEqualTo new QuadOffset(4, 8, 6))
  }

  test("QuadAddr <=> QuadOffset") {
    new QuadAddr(TopRight, BottomRight).toOffset shouldEqual
        new QuadOffset(2, 3, 2)

    new QuadOffset(2, 3, 2).toAddress(2) shouldEqual
        new QuadAddr(TopRight, BottomRight)

    new QuadOffset(0, 0, 0).toAddress(1) shouldEqual new QuadAddr(BottomLeft)
    new QuadOffset(0, 0, 0).toAddress(2) shouldEqual
        new QuadAddr(BottomLeft, BottomLeft)
  }

  test("QuadRectangle#toRectangle") {
    new QuadRectangle(new QuadOffset(2, 3, 2), new QuadOffset(2, 4, 3))
      .toRectangle shouldEqual
      (new Rectangle(new Point(0.75f, 0.5f), new Point(1, 0.75f)))
  }

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

  test("QuadTree#maxDepth") {
    new QuadLeaf(false).maxDepth shouldEqual 0

    new QuadBranch(new QuadLeaf(false),
      new QuadLeaf(false),
      new QuadLeaf(false),
      new QuadLeaf(false)).maxDepth shouldEqual 1

    new QuadBranch(new QuadLeaf(false),
      new QuadLeaf(false),
      new QuadBranch(new QuadLeaf(false),
        new QuadLeaf(false),
        new QuadLeaf(false),
        new QuadLeaf(false)),
      new QuadLeaf(false)).maxDepth shouldEqual 2
  }

  test("QuadRectangle#prune") {
    val a = new QuadRectangle(new QuadOffset(1, -1, -1),
        new QuadOffset(1, 1, 1))
    val b = new QuadRectangle(new QuadOffset(1, 0, 0).simplify,
        new QuadOffset(1, 2, 2).simplify)
    val c = new QuadRectangle(new QuadOffset(1, 1, 1),
        new QuadOffset(1, 3, 3))

    b.prune(a) shouldEqual new QuadRectangle(new QuadOffset(1, 0, 0).simplify,
        new QuadOffset(1, 1, 1))

    b.prune(c) shouldEqual new QuadRectangle(new QuadOffset(1, 1, 1),
        new QuadOffset(1, 2, 2).simplify)
  }

  test("QuadTree#shrink (grow with levels < 0)") {
    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
      new QuadLeaf(true), new QuadLeaf(false))

    q1.grow(-1, new QuadOffset(1, 0, 1), false) shouldEqual
      new QuadBranch(q1, new QuadLeaf(false),
        new QuadLeaf(false), new QuadLeaf(false))

    q1.grow(-1, new QuadOffset(1, 1, 1), false) shouldEqual
      new QuadBranch(new QuadLeaf(false), q1,
        new QuadLeaf(false), new QuadLeaf(false))

    q1.grow(-1, new QuadOffset(0, 0, 0), false) shouldEqual
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(false),
        q1, new QuadLeaf(false))
  }

  test("QuadTree#shrink - crosses existing boundaries") {
    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
      new QuadLeaf(true), new QuadLeaf(false))

    q1.grow(-1, new QuadOffset(2, 1, 1), false) shouldEqual
      new QuadBranch(new QuadLeaf(false),
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(false),
          new QuadLeaf(true), new QuadLeaf(false)),
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
          new QuadLeaf(false), new QuadLeaf(false)),
        new QuadLeaf(false))
  }

  test("QuadTree#shrink - new location partially out of bounds") {
    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
      new QuadLeaf(true), new QuadLeaf(false))

    q1.grow(-1, new QuadOffset(2, 3, 3), false) shouldEqual
      new QuadBranch(new QuadLeaf(false),
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
          new QuadLeaf(false), new QuadLeaf(false)),
        new QuadLeaf(false), new QuadLeaf(false))
  }

  test("QuadTree#shrink - new location is axis-aligned") {
    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
      new QuadLeaf(true), new QuadLeaf(false))

    new QuadLeaf(true).grow(-2, new QuadOffset(1, 1, 1), false) shouldEqual
      new QuadBranch(new QuadLeaf(false),
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(false),
          new QuadLeaf(true), new QuadLeaf(false)),
        new QuadLeaf(false), new QuadLeaf(false))
  }

  test("QuadTree#grow - offset.depth > maxDepth") {
    new QuadLeaf(true).grow(-1, new QuadOffset(2, 1, 1), false) shouldEqual
      new QuadBranch(
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(false),
          new QuadLeaf(false), new QuadLeaf(true)),
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(false),
          new QuadLeaf(true), new QuadLeaf(false)),
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
          new QuadLeaf(false), new QuadLeaf(false)),
        new QuadBranch(new QuadLeaf(true), new QuadLeaf(false),
          new QuadLeaf(false), new QuadLeaf(false)))
  }

  test("QuadTree#grow") {
    // 4x4 checkerboard
    val q1 = new QuadBranch(
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
        new QuadLeaf(true), new QuadLeaf(false)),
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
        new QuadLeaf(true), new QuadLeaf(false)),
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
        new QuadLeaf(true), new QuadLeaf(false)),
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
        new QuadLeaf(true), new QuadLeaf(false)))

    // zoom in on bottom left squares
    q1.grow(2, new QuadOffset(0, 0, 0), false) shouldEqual new QuadLeaf(true)
    q1.grow(2, new QuadOffset(0, -1, 0), false) shouldEqual new QuadLeaf(false)
    q1.grow(2, new QuadOffset(0, 0, -1), false) shouldEqual new QuadLeaf(false)
    q1.grow(2, new QuadOffset(0, -1, -1), false) shouldEqual new QuadLeaf(true)

    // zoom in on bottom left quadrant
    q1.grow(1, QuadOffset.zero, false) shouldEqual
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
        new QuadLeaf(true), new QuadLeaf(false))
  }
}

