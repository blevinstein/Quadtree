package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class QuadTreeTest extends FunSuite with Matchers {
  test("QuadTree.Builder") {
    (new QuadTree.Builder().build shouldEqual new QuadLeaf(Material.Empty))

    (new QuadTree.Builder().add(new QuadAddr(TopLeft), Material.Full).build
      shouldEqual
        new QuadBranch(new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Empty)))

    (new QuadTree.Builder()
        .add(new QuadAddr(TopLeft), Material.Full)
        .add(new QuadAddr(TopRight, TopLeft), Material.Full)
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

  test("QuadTree.approx") {
    (QuadTree.approx(0, (_) => Material.Full)
      shouldEqual
        new QuadLeaf(Material.Full))

    (QuadTree.approx(1, (p) =>
        if(p.x + p.y >= 1) Material.Full else Material.Empty)
      shouldEqual
        new QuadBranch(
          new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Full)))

    (QuadTree.approx(2, (p) =>
        if(p.x + p.y >= 1) Material.Full else Material.Empty)
      shouldEqual
        new QuadBranch(
          new QuadBranch(
            new QuadLeaf(Material.Full),
            new QuadLeaf(Material.Full),
            new QuadLeaf(Material.Empty),
            new QuadLeaf(Material.Full)),
          new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Empty),
          new QuadBranch(
            new QuadLeaf(Material.Full),
            new QuadLeaf(Material.Full),
            new QuadLeaf(Material.Empty),
            new QuadLeaf(Material.Full))))

    // tests merging
    (QuadTree.approx(3, (p) =>
        if (p.x > 0.5f) Material.Full else Material.Empty)
      shouldEqual
        new QuadBranch(
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Full),
          new QuadLeaf(Material.Empty),
          new QuadLeaf(Material.Full)))
  }

  test("QuadTree.apply") {
    val andFunc = QuadTree.apply((m1, m2) =>
        if (m1 == Material.Full && m2 == Material.Full)
          Material.Full
        else Material.Empty) _
    val orFunc = QuadTree.apply((m1, m2) =>
        if (m1 == Material.Full || m2 == Material.Full)
          Material.Full
        else Material.Empty) _

    val q1 = new QuadBranch(
      new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Full),
      new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Full))
    val q2 = new QuadBranch(
      new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Full),
      new QuadLeaf(Material.Full))
    val q3 = new QuadLeaf(Material.Empty)
    val q4 = new QuadLeaf(Material.Full)

    andFunc(q1, q2) shouldEqual
      new QuadBranch(
        new QuadLeaf(Material.Empty),
        new QuadLeaf(Material.Empty),
        new QuadLeaf(Material.Empty),
        new QuadLeaf(Material.Full))
    andFunc(q1, q4) shouldEqual q1
    andFunc(q1, q3) shouldEqual q3

    orFunc(q1, q2) shouldEqual
      new QuadBranch(
        new QuadLeaf(Material.Empty),
        new QuadLeaf(Material.Full),
        new QuadLeaf(Material.Full),
        new QuadLeaf(Material.Full))
    orFunc(q1, q3) shouldEqual q1
    orFunc(q1, q4) shouldEqual q4
  }

  test ("QuadTree#getMaterial") {
    val q1 = new QuadBranch(
      new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Full),
      new QuadLeaf(Material.Empty),
      new QuadLeaf(Material.Full))

    // getMaterial by Point
    q1.getMaterial(new Point(0.1f, 0.1f)) shouldEqual Material.Empty
    q1.getMaterial(new Point(0.9f, 0.1f)) shouldEqual Material.Full

    // getMaterial by address
    q1.getMaterial(new QuadAddr(BottomLeft)) shouldEqual Material.Empty
    q1.getMaterial(new QuadAddr(BottomRight)) shouldEqual Material.Full
    q1.getMaterial(new QuadAddr(BottomRight, BottomLeft)) shouldEqual Material.Full
  }
}

