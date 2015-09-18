package com.blevinstein.qt

object QuadRectangle {
  val unit = new QuadRectangle(new QuadOffset(0, 0, 0), new QuadOffset(0, 1, 1))
}
class QuadRectangle(val min: QuadOffset, val max: QuadOffset) {
  def toRectangle: Rectangle = new Rectangle(
      new Point(1f * min.x / (1 << min.depth), 1f * min.y / (1 << min.depth)),
      new Point(1f * max.x / (1 << max.depth), 1f * max.y / (1 << max.depth)))

  // Returns the intersection of two QuadRectangles
  def prune(other: QuadRectangle): QuadRectangle = {
    def min2d(a: QuadOffset, b: QuadOffset): QuadOffset = {
      QuadOffset.normalize(a, b) match {
        case (maxDepth, aNorm, bNorm) =>
          new QuadOffset(maxDepth,
            math.min(aNorm.x, bNorm.x),
            math.min(aNorm.y, bNorm.y)).simplify
      }
    }
    def max2d(a: QuadOffset, b: QuadOffset): QuadOffset = {
      QuadOffset.normalize(a, b) match {
        case (maxDepth, aNorm, bNorm) =>
          new QuadOffset(maxDepth,
            math.max(aNorm.x, bNorm.x),
            math.max(aNorm.y, bNorm.y)).simplify
      }
    }
    new QuadRectangle(max2d(min, other.min), min2d(max, other.max))
  }

  // TODO def allAddresses(depth: Int): List[QuadAddr] = {

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

  override def hashCode : Int =
    31 * (min.hashCode +
      31 * max.hashCode)

  override def equals(o: Any): Boolean = o match {
    case other: QuadRectangle => min == other.min && max == other.max
    case _ => false
  }

  override def toString: String = s"QuadRectangle($min, $max)"
}
