package com.blevinstein.qt

import com.blevinstein.geom.Rectangle

import scala.language.implicitConversions

// Represents an address within a QuadTree as a List[Quadrant].
//
// The size of the leaf node addressed can be inferred from the length of the
// address.
//
// For example, the top left quadrant of the bottom right quadrant would be
// expressed as:
// new QuadAddr(Quadrant.BottomRight, Quadrant.TopLeft)
object QuadAddr {
  val empty = new QuadAddr()
  implicit def toQuadrantList(addr: QuadAddr): List[Quadrant] = addr.quads
}
class QuadAddr(val quads: List[Quadrant]) {
  def this(someQuads: Quadrant*) = this(someQuads.toList)
  // copy constructor
  def this(addr: QuadAddr) = this(addr.quads)

  def toQuadRectangle: QuadRectangle = {
    val sideLen = QuadLen(1, -this.length)
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
        QuadLen(if (quadrant.x) 1 else 0, -(i + 1)),
        QuadLen(if (quadrant.y) 1 else 0, -(i + 1)))
    }
    bottomLeft
  }

  // delegate to QuadRectangle
  def touches(other: QuadAddr): Boolean =
      this.toQuadRectangle touches other.toQuadRectangle

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

