package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class QuadRectangleTest extends FunSuite with Matchers {
  test("QuadRectangle#toRectangle") {
      (new QuadRectangle(
          QuadOffset(QuadLen(3, -2), QuadLen(1, -1)),
          QuadOffset(QuadLen(1, 0), QuadLen(3, -2)))
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
        QuadOffset(QuadLen.zero, QuadLen.half),
        QuadOffset(QuadLen.half, QuadLen.one))
    val bottomRight = new QuadRectangle(
        QuadOffset(QuadLen.half, QuadLen.zero),
        QuadOffset(QuadLen.one, QuadLen.half))

    val aWithinB = topLeft within bottomRight

    aWithinB shouldEqual new QuadRectangle(
      QuadOffset(QuadLen.half, QuadLen(1, -2)),
      QuadOffset(QuadLen(3, -2), QuadLen.half))

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
        QuadOffset(QuadLen(1, -2), QuadLen(1, -3)),
        QuadOffset(QuadLen(3, -2), QuadLen(5, -3)))

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
        QuadOffset(QuadLen(3, -2), QuadLen(3, -2)))

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
        QuadOffset(QuadLen(1, -2), QuadLen(1, -2)),
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
        QuadOffset(QuadLen(0), QuadLen(2).simplify),
        QuadOffset(QuadLen(2).simplify, QuadLen(4).simplify))

    rect.toAddressList.toSet shouldEqual Set()
  }

  test("QuadRectangle#toAddressList large") {
    val rect = new QuadRectangle(QuadOffset.zero,
        QuadOffset(QuadLen(2), QuadLen(2)))

    rect.toAddressList.toSet shouldEqual Set(new QuadAddr())
  }
}
