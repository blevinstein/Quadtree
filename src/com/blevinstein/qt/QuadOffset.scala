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

  // d = depth (specificity) of the node at the address
  def toAddress(d: Int): QuadAddr = {
    require(x < (1 << depth))
    require(y < (1 << depth))
    var currentX = x
    var currentY = y
    var addr = new QuadAddr()
    for (currentDepth <- 0 until d) {
      // calculate grid size from depth
      val currentGridSize = 1 << (depth - 1 - currentDepth)
      // find which quadrant we are in, and subtract from currentX/Y
      val quadrant = new Quadrant(currentX >= currentGridSize,
        currentY >= currentGridSize)
      currentX = currentX - (if (quadrant.x) currentGridSize else 0)
      currentY = currentY - (if (quadrant.y) currentGridSize else 0)
      addr = addr + quadrant
    }
    addr
  }

  // d = depth (size) of the node to be outlined
  def toRectangle(d: Int): Rectangle = {
    val bottomLeft = new Point(1f * x / (1 << depth), 1f * y / (1 << depth))
    val size = new Point(1f / (1 << d), 1f / (1 << d))
    new Rectangle(bottomLeft, bottomLeft + size)
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

  def simplify: QuadOffset = if (depth > 0 && x % 2 == 0 && y % 2 == 0) {
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
