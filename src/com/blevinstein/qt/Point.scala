package com.blevinstein.qt

object Point {
  val zero = new Point(0, 0)
}
// scalastyle:off method.name spaces.after.plus
class Point(val x: Float, val y: Float) {
  def +(other: Point): Point = new Point(x + other.x, y + other.y)
  def -(other: Point): Point = new Point(x - other.x, y - other.y)
  def *(k: Float): Point = new Point(x * k, y * k)
  def /(k: Float): Point = new Point(x / k, y / k)
  def %(k: Float): Point = new Point(x % k, y % k)
  def mag: Float = math.sqrt(x * x + y * y).toFloat

  override def toString: String = s"($x, $y)"
}
