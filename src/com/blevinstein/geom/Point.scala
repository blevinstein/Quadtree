package com.blevinstein.geom

object Point {
  val zero = new Point(0, 0)
}
class Point(val x: Float, val y: Float) {
  def +(other: Point): Point = new Point(x + other.x, y + other.y)
  def -(other: Point): Point = new Point(x - other.x, y - other.y)
  def *(k: Float): Point = new Point(x * k, y * k)
  def /(k: Float): Point = new Point(x / k, y / k)
  def %(k: Float): Point = new Point(x % k, y % k)
  def mag: Float = math.sqrt(x * x + y * y).toFloat

  def *(other: Point): Point = new Point(x * other.x, y * other.y)
  def /(other: Point): Point = new Point(x / other.x, y / other.y)

  def xComp: Point = new Point(x, 0)
  def yComp: Point = new Point(0, y)

  // Transforms [this] from unit rectangle to [rect]
  def within(rect: Rectangle): Point =
      this * rect.size + rect.min

  // Returns [this] in the coordinate system of [rect]
  // Inverse of [within]
  def withRespectTo(rect: Rectangle): Point =
      (this - rect.min) / rect.size

  override def hashCode: Int =
    31 * (x.hashCode +
      31 * y.hashCode)
  override def equals(o: Any): Boolean = o match {
    case other: Point => x == other.x && y == other.y
    case _ => false
  }

  override def toString: String = s"($x, $y)"
}
