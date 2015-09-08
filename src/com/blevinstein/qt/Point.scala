package com.blevinstein.qt

import com.blevinstein.qt.Quadrant._

object Point {
  val zero = new Point(0, 0)
  def zoomFunc(quad : Quadrant): (Point => Point) = {
    quad match {
      case TopLeft => (p) => p * 2
      case TopRight => (p) => (p - new Point(0.5f, 0)) * 2
      case BottomLeft => (p) => (p - new Point(0, 0.5f)) * 2
      case BottomRight => (p) => (p - new Point(0.5f, 0.5f)) * 2
    }
  }
}
// scalastyle:off
class Point(val x: Float, val y: Float) {
  def +(other: Point): Point = new Point(x + other.x, y + other.y)
  def -(other: Point): Point = new Point(x - other.x, y - other.y)
  def *(k: Float): Point = new Point(x * k, y * k)
  def /(k: Float): Point = new Point(x / k, y / k)
  def %(k: Float): Point = new Point(x % k, y % k)
}
