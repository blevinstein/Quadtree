package com.blevinstein.qt

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
  val zero = new QuadOffset(0, 0, 0)
}
class QuadOffset(val depth: Int, val x: Int, val y: Int) {
  // copy constructor
  def this(offset: QuadOffset) = this(offset.depth, offset.x, offset.y)

  def toAddress(depth: Int): QuadAddr = {
    require(x < (1 << depth))
    require(y < (1 << depth))
    var currentX = x
    var currentY = y
    var currentDepth = 1
    var currentGridSize = 1 << (depth - 1)
    var addr = new QuadAddr()
    while (currentX > 0 || currentY > 0) {
      val quadrant = new Quadrant(currentX >= currentGridSize,
        currentY >= currentGridSize)
      currentX = currentX - (if (quadrant.x) currentGridSize else 0)
      currentY = currentY - (if (quadrant.y) currentGridSize else 0)
      addr = addr + quadrant
      // update depth and grid size
      currentDepth = currentDepth + 1
      currentGridSize = currentGridSize >> 1
    }
    addr
  }

  // Operators

  // scalastyle:off method.name spaces.after.plus
  def +(other: QuadOffset): QuadOffset = {
    val maxDepth = math.max(depth, other.depth)
    val normed = atDepth(maxDepth)
    val otherNormed = other.atDepth(maxDepth)
    new QuadOffset(maxDepth,
      normed.x + otherNormed.x,
      normed.y + otherNormed.y).simplify
  }

  def -(other: QuadOffset): QuadOffset = {
    val maxDepth = math.max(depth, other.depth)
    val normed = atDepth(maxDepth)
    val otherNormed = other.atDepth(maxDepth)
    new QuadOffset(maxDepth,
      normed.x - otherNormed.x,
      normed.y - otherNormed.y).simplify
  }

  def *(k: Int): QuadOffset = new QuadOffset(depth, x*k, y*k).simplify

  def atDepth(newDepth: Int): QuadOffset = {
    require(newDepth >= depth)
    new QuadOffset(newDepth,
      x * (1 << (newDepth - depth)),
      y * (1 << (newDepth - depth)))
  }

  def simplify: QuadOffset = if (x % 2 == 0 && y % 2 == 0) {
      new QuadOffset(depth - 1, x / 2, y / 2).simplify
    } else {
      this
    }

  // Like equals, but simplifies before checking equality
  // NOTE: this could work for equals, but would require that we simplify before
  //   calculating hashCode. Might not be efficient.
  def isEqualTo(other: QuadOffset): Boolean = {
    val thisSimple = simplify
    val otherSimple = other.simplify
    thisSimple == otherSimple
  }

  override def hashCode: Int =
    31 * (depth.hashCode +
      31 * (x.hashCode +
        31 * (y.hashCode)))

  override def equals(o: Any): Boolean = o match {
    case other: QuadOffset => {
      depth == other.depth &&
        x == other.x &&
        y == other.y
    }
    case _ => false
  }

  override def toString: String = s"($x / 2^$depth, $y / 2^$depth)"
}
