package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

import org.scalatest._

class QuadLenTest extends FunSuite with Matchers {
  // TODO: test QuadLen#simplify

  test("QuadLen comparisons") {
    // basics: 3/4 and 1/4
    QuadLen(3, -2) > QuadLen.half shouldEqual true
    QuadLen(1, -2) > QuadLen.half shouldEqual false
    QuadLen(3, -2) > QuadLen.zero shouldEqual true
    QuadLen(1, -2) > QuadLen.zero shouldEqual true
    QuadLen(3, -2) > QuadLen.one shouldEqual false
    QuadLen(1, -2) > QuadLen.one shouldEqual false

    // equality
    QuadLen(1, -1) > QuadLen.half shouldEqual false
    QuadLen(1, -1) >= QuadLen.half shouldEqual true
    QuadLen(1, -1) < QuadLen.half shouldEqual false
    QuadLen(1, -1) <= QuadLen.half shouldEqual true
  }

  test("QuadLen.approx") {
    QuadLen.approx(0.5f, -6) shouldEqual QuadLen(1, -1)
    QuadLen.approx(0.25f, -6) shouldEqual QuadLen(1, -2)
    QuadLen.approx(0.75f, -6) shouldEqual QuadLen(3, -2)

    QuadLen.approx(1.5f, -6) shouldEqual QuadLen(3, -1)
    QuadLen.approx(2.5f, -6) shouldEqual QuadLen(5, -1)
    QuadLen.approx(-1.5f, -6) shouldEqual QuadLen(-3, -1)
    QuadLen.approx(-2.5f, -6) shouldEqual QuadLen(-5, -1)
  }

  test("QuadLen#truncatePerfect") {
    QuadLen(7, -3).truncatePerfect shouldEqual QuadLen(1, -1)
    QuadLen(3, -3).truncatePerfect shouldEqual QuadLen(1, -2)
    QuadLen(3, -4).truncatePerfect shouldEqual QuadLen(1, -3)

    QuadLen(9).truncatePerfect shouldEqual QuadLen(8)
    QuadLen(8).truncatePerfect shouldEqual QuadLen(8)
    QuadLen(17).truncatePerfect shouldEqual QuadLen(16)
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
    QuadLen(1, -1).perfectLog shouldEqual Some(-1)
    QuadLen(1).perfectLog shouldEqual Some(0)
    QuadLen(4).perfectLog shouldEqual Some(2)
    QuadLen(8).perfectLog shouldEqual Some(3)
  }
}
