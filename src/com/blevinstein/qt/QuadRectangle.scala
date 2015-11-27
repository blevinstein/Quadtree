package com.blevinstein.qt

import com.blevinstein.geom.{Point,Rectangle}

// Represents a rectangle in quad space using QuadOffsets for the bottom left
// and top right corners of the rectangle.
object QuadRectangle {
  val zero = new QuadRectangle(QuadOffset.zero, QuadOffset.zero)
  val unit = new QuadRectangle(QuadOffset.zero, QuadOffset.one)

  def approx(min: Point, max: Point, resolution: Int): QuadRectangle = {
    new QuadRectangle(
        QuadOffset.approx(min, resolution),
        QuadOffset.approx(max, resolution))
  }
}
class QuadRectangle(val min: QuadOffset, val max: QuadOffset) {
  def this(max: QuadOffset) = this(QuadOffset.zero, max)

  require(max.x >= min.x, s"max.x ${max.x} < min.x ${min.x}")
  require(max.y >= min.y, s"max.y ${max.y} < min.y ${min.y}")

  val size: QuadOffset = max - min
  val isEmpty: Boolean = size.x.isZero || size.y.isZero

  // Returns true if [this] QuadRectangle has equal sides of the form (1 << x).
  def isPerfectSquare: Boolean = !perfectLog.isEmpty
  // If [this] has equal sides of the form (1 << x), returns x.
  def perfectLog: Option[Int] = size.perfectLog

  def toRectangle: Rectangle = new Rectangle(
      new Point(1f * min.x.toFloat, 1f * min.y.toFloat),
      new Point(1f * max.x.toFloat, 1f * max.y.toFloat))

  // Recursively generate a List of addresses covered by this rectangle.
  def toAddressList: List[QuadAddr] = {
    def splitOnX(xCoord: QuadLen): List[QuadAddr] =
        new QuadRectangle(min, new QuadOffset(xCoord, max.y)).toAddressList ++
            new QuadRectangle(new QuadOffset(xCoord, min.y), max).toAddressList
    def splitOnY(yCoord: QuadLen): List[QuadAddr] =
        new QuadRectangle(min, new QuadOffset(max.x, yCoord)).toAddressList ++
            new QuadRectangle(new QuadOffset(min.x, yCoord), max).toAddressList

    // Grid size is determined by three limitations:
    // - A QuadLeaf must be grid aligned
    // - A QuadLeaf must have sides of power-of-two length
    val xGridSize = QuadLen.min(
        new QuadLen(1, min.x.minExp), // grid aligned
        size.x.truncatePerfect)       // power-of-two length
    val yGridSize = QuadLen.min(
        new QuadLen(1, min.y.minExp), // grid aligned
        size.y.truncatePerfect)       // power-of-two length

    if (isEmpty) {
      List()
    } else if (size == new QuadOffset(xGridSize, yGridSize) &&
        isPerfectSquare) {
      // base case: grid-aligned square
      if (min.isInUnitRectangle) {
        List(min.toAddress(-size.perfectLog.get))
      } else {
        List()
      }
    } else if (xGridSize < size.x) {
      // Necessary grid-aligned split on x
      splitOnX(min.x + xGridSize)
    } else if (yGridSize < size.y) {
      // Necessary grid-aligned split on y
      splitOnY(min.y + yGridSize)
    } else if (size.y < size.x) {
      splitOnX(min.x + yGridSize)
    } else if (size.x < size.y) {
      splitOnY(min.y + xGridSize)
    } else {
      throw new IllegalStateException(
          s"isAddressList $this grid $xGridSize $yGridSize")
    }
  }

  // Transforms [this] from coords of unit rectangle to coords of [other].
  def within(other: QuadRectangle): QuadRectangle =
      new QuadRectangle(min within other, max within other)

  // Inverse of [within]. Transforms [this] such that it is expressed in the
  // coords of [other], with [other.min] at the origin, and [other.max] at
  // [QuadOffset.one].
  def withRespectTo(other: QuadRectangle): QuadRectangle =
      new QuadRectangle(min withRespectTo other, max withRespectTo other)

  def touches(other: QuadRectangle): Boolean =
    min.x <= other.max.x && max.x >= other.min.x &&
        min.y <= other.max.y && max.y >= other.min.y

  // Returns the intersection of two QuadRectangles
  def intersect(other: QuadRectangle): QuadRectangle = {
    def min2d(a: QuadOffset, b: QuadOffset): QuadOffset = new QuadOffset(
        if (a.x < b.x) a.x else b.x,
        if (a.y < b.y) a.y else b.y)
    def max2d(a: QuadOffset, b: QuadOffset): QuadOffset = new QuadOffset(
        if (a.x > b.x) a.x else b.x,
        if (a.y > b.y) a.y else b.y)
    val newMin = max2d(min, other.min)
    val newMax = min2d(max, other.max)
    if (newMax.x < newMin.x || newMax.y < newMin.y) {
      // Don't create an invalid rectangle
      QuadRectangle.zero
    } else {
      new QuadRectangle(newMin, newMax)
    }
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
