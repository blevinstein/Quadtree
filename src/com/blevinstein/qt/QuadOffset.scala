package com.blevinstein.qt

import com.blevinstein.geom.Point

import scala.language.implicitConversions

/**
 * Represents an offset between quadtrees *exactly*.
 * More accurate than a Point, which stores an offset as (x: Float, y: Float).
 *
 * QuadOffset(d, x, y) * r approximately equal to Point(x / 2^d, y / 2^d)
 * where r is the bounding rectangle of the root of the QuadTree that is
 * addressed by the QuadOffset.
 *
 * In a QuadBranch of depth d, the side of each quadrant is (1 / 2^d). So, we
 * can represent the corner of any quadrant exactly with a QuadOffset of the
 * appropriate depth.
 */
object QuadOffset {
  val zero = new QuadOffset(QuadLen.zero, QuadLen.zero)
  val half = new QuadOffset(QuadLen.half, QuadLen.half)
  val one = new QuadOffset(QuadLen.one, QuadLen.one)

  implicit def toPoint(offset: QuadOffset): Point = offset.toPoint

  def approx(point: Point, resolution: Int): QuadOffset =
      new QuadOffset(
          QuadLen.approx(point.x, resolution),
          QuadLen.approx(point.y, resolution))
}
class QuadOffset(val x: QuadLen, val y: QuadLen) {
  val minExp = x minExp y

  // Returns the x component of [this] as a QuadOffset
  def xComp: QuadOffset = new QuadOffset(x, QuadLen.zero)

  // Returns the y component of [this] as a QuadOffset
  def yComp: QuadOffset = new QuadOffset(QuadLen.zero, y)

  // Transforms [this] from unit rectangle to [rect]
  def within(rect: QuadRectangle): QuadOffset =
      (this << rect.perfectLog.get) + rect.min

  // Returns [this] in the coordinate system of [rect]
  // Inverse of [within]
  def withRespectTo(rect: QuadRectangle): QuadOffset =
      (this - rect.min) >> rect.perfectLog.get

  def isInUnitRectangle: Boolean = x >= QuadLen.zero && x < QuadLen.one &&
      y >= QuadLen.zero && y < QuadLen.one

  def toPoint: Point = new Point(x.toFloat, y.toFloat)

  // Returns an address of the given length
  def toAddress(length: Int): QuadAddr = {
    require(length >= 0, "negative length specified")
    // TODO: remove this requirement? generalize QuadAddr?
    require(isInUnitRectangle)

    var currentX = x
    var currentY = y
    var addr = QuadAddr.empty
    for (currentDepth <- 0 until length) {
      val isTop = currentY >= QuadLen.half
      val isRight = currentX >= QuadLen.half

      // Add one quadrant to address
      addr += new Quadrant(isRight, isTop)
      // Subtract new origin
      if (isTop) currentY -= QuadLen.half
      if (isRight) currentX -= QuadLen.half
      // Zoom in
      currentX <<= 1
      currentY <<= 1
    }
    addr
  }

  // For "perfect" vectors, which can be represented as (1 << x, 1 << x),
  // returns x.
  def perfectLog: Option[Int] = (x.perfectLog, y.perfectLog) match {
    case (Some(a), Some(b)) if a == b => Some(a)
    case _ => None
  }

  // Operators

  def +(other: QuadOffset): QuadOffset =
      new QuadOffset(x + other.x, y + other.y)

  def -(other: QuadOffset): QuadOffset =
      new QuadOffset(x - other.x, y - other.y)

  def *(k: Int): QuadOffset = new QuadOffset(x*k, y*k)

  def unary_- : QuadOffset = new QuadOffset(-x, -y)

  // Shift operators are used for scaling by powers of 2.
  def <<(levels: Int): QuadOffset = new QuadOffset(x << levels, y << levels)
  def >>(levels: Int): QuadOffset = new QuadOffset(x >> levels, y >> levels)

  override def hashCode: Int =
      31 * (x.hashCode +
          31 * y.hashCode)

  override def equals(o: Any): Boolean = o match {
    case other: QuadOffset => x == other.x && y == other.y
    case _ => false
  }

  override def toString: String = s"($x, $y)"
}
