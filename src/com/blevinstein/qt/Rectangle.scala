package com.blevinstein.qt

object Rectangle {
  val unit = new Rectangle(Point.zero, new Point(1, 1))
}
// scalastyle:off method.name
class Rectangle(val min: Point, val max: Point) {
  def *(k: Float): Rectangle = new Rectangle(min * k, max * k)
  def +(p: Point): Rectangle = new Rectangle(min + p, max + p)
  def -(p: Point): Rectangle = new Rectangle(min - p, max - p)

  val center = (min + max) / 2

  def contains(p: Point): Boolean = {
    p.x > min.x && p.x < max.x && p.y > min.y && p.y < max.y
  }

  def getQuadrant(quadrant : Quadrant): Rectangle = quadrant match {
    case Quadrant.TopLeft => new Rectangle(min, center)
    case Quadrant.TopRight =>
        new Rectangle(new Point(center.x, min.y), new Point(max.x, center.y))
    case Quadrant.BottomLeft =>
        new Rectangle(new Point(min.x, center.y), new Point(center.x, max.y))
    case Quadrant.BottomRight => new Rectangle(center, max)
  }
}
