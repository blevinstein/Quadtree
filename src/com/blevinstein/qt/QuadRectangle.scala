package com.blevinstein.qt

object QuadRectangle {
  val unit = new QuadRectangle(new QuadOffset(0, 0, 0), new QuadOffset(0, 1, 1))
}
class QuadRectangle(val min: QuadOffset, val max: QuadOffset) {
  def toRectangle: Rectangle = new Rectangle(
      new Point(1f * min.x / (1 << min.depth), 1f * min.y / (1 << min.depth)),
      new Point(1f * max.x / (1 << max.depth), 1f * max.y / (1 << max.depth)))

  // Operators

  def +(offset: QuadOffset): QuadRectangle =
      new QuadRectangle(min + offset, max + offset)
  def -(offset: QuadOffset): QuadRectangle =
      new QuadRectangle(min - offset, max - offset)
  def *(k: Int): QuadRectangle =
      new QuadRectangle(min * k, max * k)
  def <<(levels: Int): QuadRectangle =
      new QuadRectangle(min << levels, max << levels)
  def >>(levels: Int): QuadRectangle =
      new QuadRectangle(min >> levels, max >> levels)
}
