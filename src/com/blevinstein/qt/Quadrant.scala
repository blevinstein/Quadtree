package com.blevinstein.qt

import com.blevinstein.geom.Point

// Represents one of the four quadrants - BottomLeft, BottomRight, TopLeft, and
// TopRight.
//
// Quadrants can be referenced by name [Quadrant.BottomRight] or by x and y
// position [new Quadrant(true, false)].
object Quadrant {
  val BottomLeft = new Quadrant(false, false)
  val BottomRight = new Quadrant(true, false)
  val TopLeft = new Quadrant(false, true)
  val TopRight = new Quadrant(true, true)

  val values = List(BottomLeft, BottomRight, TopLeft, TopRight)

  def of(p: Point): Quadrant = new Quadrant(p.x >= 0.5, p.y >= 0.5)
}
class Quadrant(val x: Boolean, val y: Boolean) {
  override def hashCode: Int = 31 * (if (x) 1 else 0) + (if (y) 1 else 0)

  override def equals(o: Any): Boolean = o match {
    case other: Quadrant => x == other.x && y == other.y
    case _ => false
  }

  override def toString: String = s"($x, $y)"
}

