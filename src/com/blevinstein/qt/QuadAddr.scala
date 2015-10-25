package com.blevinstein.qt

import com.blevinstein.geom.Rectangle

import scala.language.implicitConversions

object QuadAddr {
  val empty = new QuadAddr()
  implicit def toQuadrantList(addr: QuadAddr): List[Quadrant] = addr.quads
  implicit def toQuadRectangle(addr: QuadAddr): QuadRectangle =
      addr.toQuadRectangle
}
class QuadAddr(val quads: List[Quadrant]) {
  def this(someQuads: Quadrant*) = this(someQuads.toList)
  // copy constructor
  def this(addr: QuadAddr) = this(addr.quads)

  def toQuadRectangle: QuadRectangle = {
    val sideLen = new QuadLen(1, -this.length)
    val bottomLeft = this.toOffset
    val topRight = bottomLeft + new QuadOffset(sideLen, sideLen)
    new QuadRectangle(bottomLeft, topRight)
  }

  // Returns the QuadOffset of the bottom left corner of the address.
  def toOffset: QuadOffset = {
    var bottomLeft = QuadOffset.zero
    for (i <- 0 until this.length) {
      val quadrant = this(i)
      bottomLeft = bottomLeft + new QuadOffset(
        new QuadLen(if (quadrant.x) 1 else 0, -(i + 1)),
        new QuadLen(if (quadrant.y) 1 else 0, -(i + 1)))
    }
    bottomLeft
  }

  def touches(other: QuadAddr) = {
    val thisRect: QuadRectangle = this
    val otherRect: QuadRectangle = other

    val offset = otherRect.min - thisRect.min

    -otherRect.size.x <= offset.x && offset.x <= thisRect.size.x &&
        -otherRect.size.y <= offset.y && offset.y <= thisRect.size.y
  }

  // don't implicitly delegate tail, we want a QuadAddr not List[Quadrant]
  def tail: QuadAddr = new QuadAddr(quads.tail)

  // (A inside B) iff (B isPrefixOf A)
  def isInside(other: QuadAddr): Boolean = quads startsWith other.quads

  def +(other: QuadAddr): QuadAddr = new QuadAddr(quads ++ other.quads)
  def +(quad: Quadrant): QuadAddr = new QuadAddr(quads :+ quad)

  override def hashCode: Int = quads.hashCode
  override def equals(o: Any): Boolean = o match {
    case other: QuadAddr => quads == other.quads
    case _ => false
  }

  override def toString: String = s"QuadAddr($quads)"
}

