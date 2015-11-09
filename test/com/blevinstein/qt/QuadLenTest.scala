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
  }
}
