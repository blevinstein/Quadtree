package com.blevinstein.qt

class Rectangle(val min: Point, val max: Point) {
  val center = (min + max) / 2

  def contains(p: Point): Boolean = {
    p.x > min.x && p.x < max.x && p.y > min.y && p.y < max.y
  }

  // Quadrants
  def topLeft: Rectangle = new Rectangle(min, center)
  def topRight: Rectangle =
    new Rectangle(new Point(center.x, min.y), new Point(max.x, center.y))
  def bottomLeft: Rectangle =
    new Rectangle(new Point(min.x, center.y), new Point(center.x, max.y))
  def bottomRight: Rectangle = new Rectangle(center, max)
}
