package com.blevinstein.qt

import scala.language.implicitConversions

object QuadAddr {
  // delegate to quads
  implicit def toQuadrantList(addr: QuadAddr): List[Quadrant] = addr.quads
}
class QuadAddr(val quads: List[Quadrant]) {
  def this(someQuads: Quadrant*) = this(someQuads.toList)
  // copy constructor
  def this(addr: QuadAddr) = this(addr.quads)

  /**
   * Returns the QuadOffset of the bottom left corner of the address.
   */
  def toOffset: QuadOffset = {
    var bottomLeft = QuadOffset.zero
    for (i <- 0 until this.length) {
      val quadrant = this(i)
      bottomLeft = bottomLeft + new QuadOffset(i + 1,
        if (quadrant.x) 1 else 0,
        if (quadrant.y) 1 else 0)
    }
    bottomLeft
  }

  def toRectangle: Rectangle = {
    val bottomLeft = this.toOffset
    val topRight = bottomLeft + new QuadOffset(this.length, 1, 1)
    new QuadRectangle(bottomLeft, topRight).toRectangle
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

