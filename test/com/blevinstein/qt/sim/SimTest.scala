package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadOffset,QuadRectangle}

import org.scalatest._

class SimTest extends FunSuite with Matchers {
  test("QuadZone.around(QuadOffset)") {
    QuadZone.around(QuadOffset.half) shouldEqual new QuadZone(0, 0)
    QuadZone.around(QuadOffset.half + QuadOffset.one) shouldEqual
        new QuadZone(1, 1)
  }
  test("QuadZone.around(QuadRectangle)") {
    val rect = new QuadRectangle(
        QuadOffset.zero,
        QuadOffset.one + QuadOffset.half)
    QuadZone.around(rect).toSet shouldEqual
        Set(new QuadZone(0, 0),
            new QuadZone(0, 1),
            new QuadZone(1, 0),
            new QuadZone(1, 1))
  }
}
