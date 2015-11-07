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
          totalArea += (if (bool) addr.toQuadRectangle.toRectangle.area else 0)
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

  test("QuadAddr#touches") {
    new QuadAddr(TopLeft) touches new QuadAddr(TopRight) shouldEqual true
    new QuadAddr(TopLeft, TopRight) touches new QuadAddr(TopRight) shouldEqual
        true
    new QuadAddr(TopLeft, TopRight) touches new QuadAddr(BottomLeft) shouldEqual
        false
  }

  test("QuadRectangle#toRectangle") {
      (new QuadRectangle(
          new QuadOffset(new QuadLen(3, -2), new QuadLen(1, -1)),
          new QuadOffset(new QuadLen(1, 0), new QuadLen(3, -2)))
          .toRectangle
      shouldEqual
          new Rectangle(new Point(0.75f, 0.5f), new Point(1, 0.75f)))
  }

  test("QuadRectangle#intersect") {
    val a = new QuadRectangle(-QuadOffset.half, QuadOffset.half)
    val b = new QuadRectangle(QuadOffset.zero, QuadOffset.one)
    val c = new QuadRectangle(QuadOffset.half, QuadOffset.one + QuadOffset.half)

    b intersect a shouldEqual new QuadRectangle(QuadOffset.zero, QuadOffset.half)

    b intersect c shouldEqual new QuadRectangle(QuadOffset.half, QuadOffset.one)
  }

  test("QuadRectangle#within, #withRespectTo") {
    val topLeft = new QuadRectangle(
        new QuadOffset(QuadLen.zero, QuadLen.half),
        new QuadOffset(QuadLen.half, QuadLen.one))
    val bottomRight = new QuadRectangle(
        new QuadOffset(QuadLen.half, QuadLen.zero),
        new QuadOffset(QuadLen.one, QuadLen.half))

    val aWithinB = topLeft within bottomRight

    aWithinB shouldEqual new QuadRectangle(
      new QuadOffset(QuadLen.half, new QuadLen(1, -2)),
      new QuadOffset(new QuadLen(3, -2), QuadLen.half))

    aWithinB withRespectTo bottomRight shouldEqual topLeft

    // Both within and withRespectTo become the identity function on the unit
    // rectangle.
    topLeft within QuadRectangle.unit shouldEqual topLeft
    bottomRight withRespectTo QuadRectangle.unit shouldEqual bottomRight

    // Any rectangle withRespectTo itself is the unit rectangle
    topLeft withRespectTo topLeft shouldEqual QuadRectangle.unit
    bottomRight withRespectTo bottomRight shouldEqual QuadRectangle.unit
  }

  test("QuadRectangle#toAddressList basic case") {
    val rect = new QuadRectangle(
        new QuadOffset(new QuadLen(1, -2), new QuadLen(1, -3)),
        new QuadOffset(new QuadLen(3, -2), new QuadLen(5, -3)))

    // - abcd
    //   yyzz
    //   yyzz
    //   efgh
    // +   |   |
    rect.toAddressList.toSet shouldEqual Set(
        new QuadAddr(TopLeft, BottomRight, BottomLeft), // a
        new QuadAddr(TopLeft, BottomRight, BottomRight), // b
        new QuadAddr(TopRight, BottomLeft, BottomLeft), // c
        new QuadAddr(TopRight, BottomLeft, BottomRight), // d
        new QuadAddr(BottomLeft, TopRight), // y
        new QuadAddr(BottomRight, TopLeft), // z
        new QuadAddr(BottomLeft, BottomRight, TopLeft), // e
        new QuadAddr(BottomLeft, BottomRight, TopRight), // f
        new QuadAddr(BottomRight, BottomLeft, TopLeft), // g
        new QuadAddr(BottomRight, BottomLeft, TopRight)) // h
  }

  test("QuadRectangle#toAddressList off-grid squares") {
    val rectOne = new QuadRectangle(
        QuadOffset.zero,
        new QuadOffset(new QuadLen(3, -2), new QuadLen(3, -2)))

    // -
    //  abc
    //  xxd
    //  xxe
    // +   |
    rectOne.toAddressList.toSet shouldEqual Set(
        new QuadAddr(BottomLeft), // x
        new QuadAddr(TopLeft, BottomLeft), // a
        new QuadAddr(TopLeft, BottomRight), // b
        new QuadAddr(TopRight, BottomLeft), // c
        new QuadAddr(BottomRight, TopLeft), // d
        new QuadAddr(BottomRight, BottomLeft)) // e

    val rectTwo = new QuadRectangle(
        new QuadOffset(new QuadLen(1, -2), new QuadLen(1, -2)),
        QuadOffset.one)

    // - axx
    //   bxx
    //   cde
    //
    // +   |
    rectTwo.toAddressList.toSet shouldEqual Set(
        new QuadAddr(TopRight), // x
        new QuadAddr(TopLeft, TopRight), // a
        new QuadAddr(TopLeft, BottomRight), // b
        new QuadAddr(BottomLeft, TopRight), // c
        new QuadAddr(BottomRight, TopLeft), // d
        new QuadAddr(BottomRight, TopRight)) // e
  }

  test("QuadRectangle#toAddressList out of bounds") {
    val rect = new QuadRectangle(
        new QuadOffset(new QuadLen(0), new QuadLen(2).simplify),
        new QuadOffset(new QuadLen(2).simplify, new QuadLen(4).simplify))

    rect.toAddressList.toSet shouldEqual Set()
  }

  test("QuadRectangle#toAddressList large") {
    val rect = new QuadRectangle(QuadOffset.zero,
        new QuadOffset(new QuadLen(2), new QuadLen(2)))

    rect.toAddressList.toSet shouldEqual Set(new QuadAddr())
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

  test("QuadTree#getGraph - fully connected graph") {
    val qt = new QuadBranch(
        new QuadLeaf(1),
        new QuadLeaf(2),
        new QuadLeaf(3),
        new QuadLeaf(4))

    val bl = new QuadAddr(BottomLeft)
    val br = new QuadAddr(BottomRight)
    val tl = new QuadAddr(TopLeft)
    val tr = new QuadAddr(TopRight)

    qt.getGraph shouldEqual Map(
        (bl, Set(tr, tl, br)),
        (br, Set(tr, tl, bl)),
        (tl, Set(tr, bl, br)),
        (tr, Set(tl, bl, br)))
  }

  test("QuadTree#getGraph - simple case") {
    val qt = new QuadBranch(
        new QuadBranch(
          new QuadLeaf(1),
          new QuadLeaf(2),
          new QuadLeaf(3),
          new QuadLeaf(4)),
        new QuadLeaf(5),
        new QuadLeaf(6),
        new QuadLeaf(7))

    val tl_bl = new QuadAddr(TopLeft, BottomLeft)
    val tl_br = new QuadAddr(TopLeft, BottomRight)
    val tl_tl = new QuadAddr(TopLeft, TopLeft)
    val tl_tr = new QuadAddr(TopLeft, TopRight)

    val bl = new QuadAddr(BottomLeft)
    val br = new QuadAddr(BottomRight)
    val tr = new QuadAddr(TopRight)

    qt.getGraph shouldEqual Map(
        (tl_bl, Set(bl, tl_br, tl_tr, tl_tl)),
        (tl_br, Set(bl, br, tr, tl_bl, tl_tr, tl_tl)),
        (tl_tl, Set(tl_bl, tl_br, tl_tr)),
        (tl_tr, Set(tl_tl, tl_br, tl_bl, tr)),
        (bl, Set(tr, tl_bl, tl_br, br)),
        (br, Set(tr, tl_br, bl)),
        (tr, Set(tl_tr, tl_br, bl, br)))
  }

  test("QuadTree#getRegions") {
    val qt = new QuadBranch(
        new QuadBranch(
          new QuadLeaf(1),
          new QuadLeaf(1),
          new QuadLeaf(2),
          new QuadLeaf(2)),
        new QuadLeaf(2),
        new QuadLeaf(2),
        new QuadLeaf(3))

    val tl_bl = new QuadAddr(TopLeft, BottomLeft)
    val tl_br = new QuadAddr(TopLeft, BottomRight)
    val tl_tl = new QuadAddr(TopLeft, TopLeft)
    val tl_tr = new QuadAddr(TopLeft, TopRight)

    val bl = new QuadAddr(BottomLeft)
    val br = new QuadAddr(BottomRight)
    val tr = new QuadAddr(TopRight)

    val regions = qt.getRegions
    regions.find { case (num: Int, _) => num == 1 }.get._2.toSet shouldEqual
        Set(tl_tl, tl_tr)
    regions.find { case (num: Int, _) => num == 2 }.get._2.toSet shouldEqual
        Set(tl_bl, tl_br, tr, bl)
    regions.find { case (num: Int, _) => num == 3 }.get._2.toSet shouldEqual
        Set(br)
  }

  test("QuadLen.approx") {
    QuadLen.approx(0.5f, -6) shouldEqual new QuadLen(1, -1)
    QuadLen.approx(0.25f, -6) shouldEqual new QuadLen(1, -2)
    QuadLen.approx(0.75f, -6) shouldEqual new QuadLen(3, -2)

    QuadLen.approx(1.5f, -6) shouldEqual new QuadLen(3, -1)
    QuadLen.approx(2.5f, -6) shouldEqual new QuadLen(5, -1)
    QuadLen.approx(-1.5f, -6) shouldEqual new QuadLen(-3, -1)
    QuadLen.approx(-2.5f, -6) shouldEqual new QuadLen(-5, -1)
  }

  test("QuadLen#truncatePerfect") {
    new QuadLen(7, -3).truncatePerfect shouldEqual new QuadLen(1, -1)

    new QuadLen(3, -3).truncatePerfect shouldEqual new QuadLen(1, -2)

    new QuadLen(3, -4).truncatePerfect shouldEqual new QuadLen(1, -3)
  }
}

