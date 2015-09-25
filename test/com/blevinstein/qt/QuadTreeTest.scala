package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}
import com.blevinstein.qt.Material.{Empty,Full}

import org.scalatest._

class QuadTreeTest extends FunSuite with Matchers {
  test("QuadTree.Builder") {
    (new QuadTree.Builder[Material](Empty).build
      shouldEqual
      new QuadLeaf(Empty))

    (new QuadTree.Builder[Material](Empty).add(new QuadAddr(TopLeft), Full)
      .build
      shouldEqual
        new QuadBranch(new QuadLeaf(Full),
          new QuadLeaf(Empty),
          new QuadLeaf(Empty),
          new QuadLeaf(Empty)))

    (new QuadTree.Builder[Material](Empty)
        .add(new QuadAddr(TopLeft), Full)
        .add(new QuadAddr(TopRight, TopLeft), Full)
        .build
      shouldEqual
        new QuadBranch(
          new QuadLeaf(Full),
          new QuadBranch(
            new QuadLeaf(Full),
            new QuadLeaf(Empty),
            new QuadLeaf(Empty),
            new QuadLeaf(Empty)),
          new QuadLeaf(Empty),
          new QuadLeaf(Empty)))
  }

  test("QuadTree.approx") {
    (QuadTree.approx(0, (_) => Full)
      shouldEqual
        new QuadLeaf(Full))

    (QuadTree.approx(1, (p) =>
        if(p.x + p.y >= 1) Full else Empty)
      shouldEqual
        new QuadBranch(
          new QuadLeaf(Full),
          new QuadLeaf(Full),
          new QuadLeaf(Empty),
          new QuadLeaf(Full)))

    (QuadTree.approx(2, (p) =>
        if(p.x + p.y >= 1) Full else Empty)
      shouldEqual
        new QuadBranch(
          new QuadBranch(
            new QuadLeaf(Full),
            new QuadLeaf(Full),
            new QuadLeaf(Empty),
            new QuadLeaf(Full)),
          new QuadLeaf(Full),
          new QuadLeaf(Empty),
          new QuadBranch(
            new QuadLeaf(Full),
            new QuadLeaf(Full),
            new QuadLeaf(Empty),
            new QuadLeaf(Full))))

    // tests merging
    (QuadTree.approx(3, (p) =>
        if (p.x > 0.5f) Full else Empty)
      shouldEqual
        new QuadBranch(
          new QuadLeaf(Empty),
          new QuadLeaf(Full),
          new QuadLeaf(Empty),
          new QuadLeaf(Full)))
  }

  test("QuadTree.merge") {
    val andFunc = QuadTree.merge((m1: Material, m2: Material) =>
        if (m1 == Full && m2 == Full)
          Full
        else Empty) _
    val orFunc = QuadTree.merge((m1: Material, m2: Material) =>
        if (m1 == Full || m2 == Full)
          Full
        else Empty) _

    val q1 = new QuadBranch(
      new QuadLeaf(Empty),
      new QuadLeaf(Full),
      new QuadLeaf(Empty),
      new QuadLeaf(Full))
    val q2 = new QuadBranch(
      new QuadLeaf(Empty),
      new QuadLeaf(Empty),
      new QuadLeaf(Full),
      new QuadLeaf(Full))
    val q3 = new QuadLeaf(Empty)
    val q4 = new QuadLeaf(Full)

    andFunc(q1, q2) shouldEqual
      new QuadBranch(
        new QuadLeaf(Empty),
        new QuadLeaf(Empty),
        new QuadLeaf(Empty),
        new QuadLeaf(Full))
    andFunc(q1, q4) shouldEqual q1
    andFunc(q1, q3) shouldEqual q3

    orFunc(q1, q2) shouldEqual
      new QuadBranch(
        new QuadLeaf(Empty),
        new QuadLeaf(Full),
        new QuadLeaf(Full),
        new QuadLeaf(Full))
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

    val q1 = new QuadBranch(new QuadLeaf(Empty), new QuadLeaf(Empty),
        new QuadLeaf(Full), new QuadLeaf(Full))
    val q2 = new QuadBranch(new QuadLeaf(Empty), new QuadLeaf(Full),
        new QuadLeaf(Empty), new QuadLeaf(Full))

    val matchFunc = QuadTree.merge((m1: Material, m2: Material) =>
        if (m1 == m2) true else false) _

    getArea(matchFunc(q1, q2)) shouldEqual 0.5f
  }

  test("QuadTree.transform and .reduce to calculate area") {
    val avgFunc = QuadTree.reduce((xs: List[Float]) => {
      require(xs.length == 4)
      xs.reduceLeft(_ + _) / 4
    }) _
    val countFunc = QuadTree.transform((m: Material) => m match {
      case Full => 1f
      case Empty => 0f
    }) _

    val q1 = new QuadBranch(new QuadLeaf(Empty), new QuadLeaf(Full),
      new QuadBranch(new QuadLeaf(Empty), new QuadLeaf(Full),
        new QuadLeaf(Full), new QuadLeaf(Empty)),
      new QuadLeaf(Empty))

    val areaOf = (tree: QuadTree[Material]) => avgFunc(countFunc(tree))

    areaOf(q1) shouldEqual 3.0/8
  }

  test("QuadTree#getData") {
    val q1 = new QuadBranch(
      new QuadLeaf(Empty),
      new QuadLeaf(Full),
      new QuadLeaf(Empty),
      new QuadLeaf(Full))

    // getData by Point
    q1.getData(new Point(0.1f, 0.1f)) shouldEqual Empty
    q1.getData(new Point(0.9f, 0.1f)) shouldEqual Full

    // getData by address
    q1.getData(new QuadAddr(BottomLeft)) shouldEqual Empty
    q1.getData(new QuadAddr(BottomRight)) shouldEqual Full
    q1.getData(new QuadAddr(BottomRight, BottomLeft)) shouldEqual Full
  }

  test("QuadOffset#simplify") {
    // explicit simplify
    new QuadOffset(4, 8, 6).simplify shouldEqual new QuadOffset(3, 4, 3)

    // equals should understand simplification
    assert(new QuadOffset(4, 8, 6) isEqualTo new QuadOffset(3, 4, 3))
    assert(new QuadOffset(3, 4, 3) isEqualTo new QuadOffset(4, 8, 6))
  }

  test("QuadAddr <=> QuadOffset") {
    (new QuadAddr(TopRight, BottomRight).toOffset
      shouldEqual new QuadOffset(2, 3, 2))

    (new QuadOffset(2, 3, 2).toAddress(2)
      shouldEqual new QuadAddr(TopRight, BottomRight))
  }

  test("QuadRectangle#toRectangle") {
    new QuadRectangle(new QuadOffset(2, 3, 2), new QuadOffset(2, 4, 3))
      .toRectangle shouldEqual
      (new Rectangle(new Point(0.75f, 0.5f), new Point(1, 0.75f)))
  }

  test("Transform") {
    val q1 = new QuadBranch(new QuadLeaf(Full),
        new QuadLeaf(Empty),
        new QuadLeaf(Empty),
        new QuadBranch(new QuadLeaf(Full),
          new QuadLeaf(Empty),
          new QuadLeaf(Empty),
          new QuadLeaf(Full)))

    Transform.rotateLeft(q1) shouldEqual
        new QuadBranch(new QuadLeaf(Empty),
          new QuadBranch(new QuadLeaf(Empty),
            new QuadLeaf(Full),
            new QuadLeaf(Full),
            new QuadLeaf(Empty)),
          new QuadLeaf(Full),
          new QuadLeaf(Empty))

    Transform.rotateRight(q1) shouldEqual
        new QuadBranch(new QuadLeaf(Empty),
          new QuadLeaf(Full),
          new QuadBranch(new QuadLeaf(Empty),
            new QuadLeaf(Full),
            new QuadLeaf(Full),
            new QuadLeaf(Empty)),
          new QuadLeaf(Empty))

    Transform.mirror(q1) shouldEqual
        new QuadBranch(new QuadLeaf(Empty),
          new QuadLeaf(Full),
          new QuadBranch(new QuadLeaf(Empty),
            new QuadLeaf(Full),
            new QuadLeaf(Full),
            new QuadLeaf(Empty)),
          new QuadLeaf(Empty))
  }

  test("QuadTree#maxDepth") {
    new QuadLeaf(Empty).maxDepth shouldEqual 0

    new QuadBranch(new QuadLeaf(Empty),
      new QuadLeaf(Empty),
      new QuadLeaf(Empty),
      new QuadLeaf(Empty)).maxDepth shouldEqual 1

    new QuadBranch(new QuadLeaf(Empty),
      new QuadLeaf(Empty),
      new QuadBranch(new QuadLeaf(Empty),
        new QuadLeaf(Empty),
        new QuadLeaf(Empty),
        new QuadLeaf(Empty)),
      new QuadLeaf(Empty)).maxDepth shouldEqual 2
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
}

