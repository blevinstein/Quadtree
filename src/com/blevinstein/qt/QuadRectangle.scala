package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}

object QuadRectangle {
  val unit = new QuadRectangle(new QuadOffset(0, 0, 0), new QuadOffset(0, 1, 1))
}
class QuadRectangle(val min: QuadOffset, val max: QuadOffset) {
  def toRectangle: Rectangle = new Rectangle(
      new Point(1f * min.fx, 1f * min.fy),
      new Point(1f * max.fx, 1f * max.fy))

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

  // Returns all addresses in the contained area at a given depth
  // TODO: add tests
  def allAddresses(depth: Int): List[QuadAddr] = {
    val minNormed = min.atDepth(depth)
    val maxNormed = max.atDepth(depth)
    var addresses = List[QuadAddr]()
    for (i <- minNormed.x until maxNormed.x) {
      for (j <- minNormed.y until maxNormed.y) {
        addresses = new QuadOffset(depth, i, j).toAddress(depth) :: addresses
      }
    }
    addresses
  }

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
