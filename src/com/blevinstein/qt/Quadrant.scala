package com.blevinstein.qt

object Quadrant extends Enumeration {
  type Quadrant = Value
  val TopLeft, TopRight, BottomLeft, BottomRight = Value
  def of(p: Point): Quadrant = {
    if (p.x < 0.5) {
      if (p.y < 0.5) {
        BottomLeft
      } else {
        TopLeft
      }
    } else {
      if (p.y < 0.5) {
        BottomRight
      } else {
        TopRight
      }
    }
  }
}
