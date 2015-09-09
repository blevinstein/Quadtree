package com.blevinstein.qt

object Quadrant {
  val BottomLeft = new Quadrant(false, false)
  val BottomRight = new Quadrant(true, false)
  val TopLeft = new Quadrant(false, true)
  val TopRight = new Quadrant(true, true)

  val values = List(BottomLeft, BottomRight, TopLeft, TopRight)

  def of(p: Point): Quadrant = new Quadrant(p.x >= 0.5, p.y >= 0.5)
}
class Quadrant(val x: Boolean, val y: Boolean)

