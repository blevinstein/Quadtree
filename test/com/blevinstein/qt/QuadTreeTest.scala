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

  // TODO: test QuadLen#simplify

  test("QuadLen comparisons") {
    // basics: 3/4 and 1/4
    new QuadLen(3, -2) > QuadLen.half shouldEqual true
    new QuadLen(1, -2) > QuadLen.half shouldEqual false
    new QuadLen(3, -2) > QuadLen.zero shouldEqual true
    new QuadLen(1, -2) > QuadLen.zero shouldEqual true
    new QuadLen(3, -2) > QuadLen.one shouldEqual false
    new QuadLen(1, -2) > QuadLen.one shouldEqual false

    // equality
    new QuadLen(1, -1) > QuadLen.half shouldEqual false
    new QuadLen(1, -1) >= QuadLen.half shouldEqual true
    new QuadLen(1, -1) < QuadLen.half shouldEqual false
    new QuadLen(1, -1) <= QuadLen.half shouldEqual true
  }

  test("QuadAddr <=> QuadOffset") {
    new QuadAddr(TopRight, BottomRight).toOffset shouldEqual
        new QuadOffset(new QuadLen(3, -2), new QuadLen(1, -1))

    (new QuadOffset(new QuadLen(3, -2), new QuadLen(1, -1)).toAddress(2)
        shouldEqual
        new QuadAddr(TopRight, BottomRight))

    QuadOffset.zero.toAddress(1) shouldEqual new QuadAddr(BottomLeft)
    QuadOffset.zero.toAddress(2) shouldEqual
        new QuadAddr(BottomLeft, BottomLeft)
  }

  test("QuadRectangle#toRectangle") {
      (new QuadRectangle(
          new QuadOffset(new QuadLen(3, -2), new QuadLen(1, -1)),
          new QuadOffset(new QuadLen(1, 0), new QuadLen(3, -2)))
          .toRectangle
      shouldEqual
          new Rectangle(new Point(0.75f, 0.5f), new Point(1, 0.75f)))
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
    val a = new QuadRectangle(-QuadOffset.half, QuadOffset.half)
    val b = new QuadRectangle(QuadOffset.zero, QuadOffset.one)
    val c = new QuadRectangle(QuadOffset.half, QuadOffset.one + QuadOffset.half)

    b.prune(a) shouldEqual new QuadRectangle(QuadOffset.zero, QuadOffset.half)

    b.prune(c) shouldEqual new QuadRectangle(QuadOffset.half, QuadOffset.one)
  }

  test("QuadTree#shrink (grow with levels < 0)") {
    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
      new QuadLeaf(true), new QuadLeaf(false))

    q1.grow(-1, new QuadOffset(QuadLen.zero, QuadLen.half), false) shouldEqual
      new QuadBranch(q1, new QuadLeaf(false),
        new QuadLeaf(false), new QuadLeaf(false))

    q1.grow(-1, QuadOffset.half, false) shouldEqual
      new QuadBranch(new QuadLeaf(false), q1,
        new QuadLeaf(false), new QuadLeaf(false))

    q1.grow(-1, QuadOffset.zero, false) shouldEqual
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(false),
        q1, new QuadLeaf(false))
  }

  test("QuadTree#shrink - crosses existing boundaries") {
    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
      new QuadLeaf(true), new QuadLeaf(false))

    q1.grow(-1, QuadOffset.half >> 1, false) shouldEqual
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

    q1.grow(-1, (QuadOffset.one * 3) >> 2, false) shouldEqual
      new QuadBranch(new QuadLeaf(false),
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
          new QuadLeaf(false), new QuadLeaf(false)),
        new QuadLeaf(false), new QuadLeaf(false))
  }

  test("QuadTree#shrink - new location is axis-aligned") {
    val q1 = new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
      new QuadLeaf(true), new QuadLeaf(false))

    new QuadLeaf(true).grow(-2, QuadOffset.half, false) shouldEqual
      new QuadBranch(new QuadLeaf(false),
        new QuadBranch(new QuadLeaf(false), new QuadLeaf(false),
          new QuadLeaf(true), new QuadLeaf(false)),
        new QuadLeaf(false), new QuadLeaf(false))
  }

  test("QuadTree#grow - offset.depth > maxDepth") {
    new QuadLeaf(true).grow(-1, QuadOffset.half >> 1, false) shouldEqual
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
    // NOTE: the "fill" argument of grow should always be the opposite of what
    //     you expect, to minimize false negatives
    q1.grow(2, QuadOffset.zero, false) shouldEqual new QuadLeaf(true)
    q1.grow(2, new QuadOffset(-QuadLen.one, QuadLen.zero), true) shouldEqual
        new QuadLeaf(false)
    q1.grow(2, new QuadOffset(QuadLen.zero, -QuadLen.one), true) shouldEqual
        new QuadLeaf(false)
    q1.grow(2, new QuadOffset(-QuadLen.one, -QuadLen.one), false) shouldEqual
      new QuadLeaf(true)

    // zoom in on bottom left quadrant
    q1.grow(1, QuadOffset.zero, false) shouldEqual
      new QuadBranch(new QuadLeaf(false), new QuadLeaf(true),
        new QuadLeaf(true), new QuadLeaf(false))
  }
}

