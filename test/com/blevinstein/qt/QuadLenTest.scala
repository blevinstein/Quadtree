package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class QuadLenTest extends FunSuite with Matchers {
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

    new QuadLen(9).truncatePerfect shouldEqual new QuadLen(8)
    new QuadLen(8).truncatePerfect shouldEqual new QuadLen(8)
    new QuadLen(17).truncatePerfect shouldEqual new QuadLen(16)
  }

  test("QuadLen#log2") {
    QuadLen.log2(0) shouldEqual None
    QuadLen.log2(1) shouldEqual Some(0)
    QuadLen.log2(2) shouldEqual Some(1)
    QuadLen.log2(4) shouldEqual Some(2)
    QuadLen.log2(5) shouldEqual None
    QuadLen.log2(8) shouldEqual Some(3)
  }

  test("QuadLen#perfectLog") {
    new QuadLen(1, -1).perfectLog shouldEqual Some(-1)
    new QuadLen(1).perfectLog shouldEqual Some(0)
    new QuadLen(4).perfectLog shouldEqual Some(2)
    new QuadLen(8).perfectLog shouldEqual Some(3)
  }
}
