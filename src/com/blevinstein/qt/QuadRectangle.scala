package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}

object QuadRectangle {
  val unit = new QuadRectangle(QuadOffset.zero, QuadOffset.one)
}
class QuadRectangle(val min: QuadOffset, val max: QuadOffset) {
  // Returns true if this QuadRectangle has equal sides of the form (1 << x).
  def isPerfectSquare: Boolean = !perfectLog.isEmpty
  // If this has equal sides of the form (1 << x), returns x.
  def perfectLog: Option[Int] = (max - min).perfectLog

  def toRectangle: Rectangle = new Rectangle(
      new Point(1f * min.x.toFloat, 1f * min.y.toFloat),
      new Point(1f * max.x.toFloat, 1f * max.y.toFloat))

  // Returns the intersection of two QuadRectangles
  def prune(other: QuadRectangle): QuadRectangle = {
    def min2d(a: QuadOffset, b: QuadOffset): QuadOffset = new QuadOffset(
        if (a.x < b.x) a.x else b.x,
        if (a.y < b.y) a.y else b.y)
    def max2d(a: QuadOffset, b: QuadOffset): QuadOffset = new QuadOffset(
        if (a.x > b.x) a.x else b.x,
        if (a.y > b.y) a.y else b.y)
    new QuadRectangle(max2d(min, other.min), min2d(max, other.max))
  }

  // Returns all addresses in the contained area at a given depth
  // TODO: add tests
  def allAddresses(depth: Int): List[QuadAddr] = {
    val stepLen = new QuadLen(1, -depth)
    var addresses = List[QuadAddr]()
    for (i <- min.x.untilBy(max.x, stepLen)) {
      for (j <- min.y.untilBy(max.y, stepLen)) {
        addresses = new QuadOffset(i, j).toAddress(depth) :: addresses
      }
    }
    addresses
  }

  // Operators

  def +(offset: QuadOffset): QuadRectangle =
      new QuadRectangle(min + offset, max + offset)
  def -(offset: QuadOffset): QuadRectangle =
      new QuadRectangle(min - offset, max - offset)
  // NOTE: * << >> all scale w.r.t. origin, NOT w.r.t. min
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
