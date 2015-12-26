package com.blevinstein.qt.sim

import com.blevinstein.geom.Point

import org.scalatest._

class DriverTest extends FunSuite with Matchers {
  test("Camera focused on (1,1)") {
    val camera = Camera.focus(new Point(1, 1), new Point(1, 1))
    // Check min/max corners of the viewport
    camera.put(new Point(0, 0)) shouldEqual new Point(0.5f, 0.5f)
    camera.put(new Point(1, 1)) shouldEqual new Point(1.5f, 1.5f)

    // Check that the camera is centered correctly
    camera.get(new Point(1, 1)) shouldEqual new Point(0.5f, 0.5f)
  }
  test("Camera zoomed out") {
    val camera = Camera.focus(new Point(5, 5), 0.1f)

    camera.put(new Point(0, 0)) shouldEqual new Point(4.95f, 4.95f)
    camera.put(new Point(1, 1)) shouldEqual new Point(5.05f, 5.05f)

    camera.get(new Point(5, 5)) shouldEqual new Point(0.5f, 0.5f)
  }
}
