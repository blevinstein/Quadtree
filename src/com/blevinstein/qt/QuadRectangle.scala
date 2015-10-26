package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}

// Represents a rectangle in quad space using QuadOffsets for the bottom left
// and top right corners of the rectangle.
object QuadRectangle {
  val unit = new QuadRectangle(QuadOffset.zero, QuadOffset.one)
}
class QuadRectangle(val min: QuadOffset, val max: QuadOffset) {
  val size: QuadOffset = max - min
  val isEmpty: Boolean = size.x.isZero || size.y.isZero

  // Returns true if [this] QuadRectangle has equal sides of the form (1 << x).
  def isPerfectSquare: Boolean = !perfectLog.isEmpty
  // If [this] has equal sides of the form (1 << x), returns x.
  def perfectLog: Option[Int] = (max - min).perfectLog

  def toRectangle: Rectangle = new Rectangle(
      new Point(1f * min.x.toFloat, 1f * min.y.toFloat),
      new Point(1f * max.x.toFloat, 1f * max.y.toFloat))

  // Recursively generate a List of addresses covered by this rectangle.
  def toAddressList: List[QuadAddr] = {
    val minXLen = QuadLen.min(new QuadLen(1, min.x.minExp), size.x, QuadLen.one)
    val minYLen = QuadLen.min(new QuadLen(1, min.y.minExp), size.y, QuadLen.one)

    if (isEmpty) {
      List()
    } else if (size == new QuadOffset(minXLen, minYLen) && size.x == size.y) {
      if (min.isInUnitRectangle) {
        List(min.toAddress(-size.perfectLog.get))
      } else {
        List()
      }
    } else if (minXLen < size.x) {
      // Split by x coord and recur
      val xCoord = min.x + minXLen
      new QuadRectangle(min, new QuadOffset(xCoord, max.y)).toAddressList ++
          new QuadRectangle(new QuadOffset(xCoord, min.y), max).toAddressList
    } else if (size.y < size.x) {
      // Split by x coord and recur
      val xCoord = min.x + minYLen
      new QuadRectangle(min, new QuadOffset(xCoord, max.y)).toAddressList ++
          new QuadRectangle(new QuadOffset(xCoord, min.y), max).toAddressList
    } else {
      // Split by y coord and recur
      val yCoord = min.y + minYLen
      new QuadRectangle(min, new QuadOffset(max.x, yCoord)).toAddressList ++
          new QuadRectangle(new QuadOffset(min.x, yCoord), max).toAddressList
    }
  }

  // Transforms [this] from coords of unit rectangle to coords of [other].
  def within(other: QuadRectangle): QuadRectangle =
      (this << other.perfectLog.get) + other.min

  // Inverse of [within]. Transforms [this] such that it is expressed in the
  // coords of [other], with [other.min] at the origin, and [other.max] at
  // [QuadOffset.one].
  def withRespectTo(other: QuadRectangle): QuadRectangle =
      (this - other.min) >> other.perfectLog.get

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
