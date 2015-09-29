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

  def normalize(a: QuadOffset, b: QuadOffset): (Int, QuadOffset, QuadOffset) = {
    val maxDepth = math.max(a.depth, b.depth)
    val aNorm = a.atDepth(maxDepth)
    val bNorm = b.atDepth(maxDepth)
    (maxDepth, aNorm, bNorm)
  }
}
class QuadOffset(val depth: Int, val x: Int, val y: Int) {
  // copy constructor
  def this(offset: QuadOffset) = this(offset.depth, offset.x, offset.y)

  def isValid: Boolean = (x < (1 << depth)) && (y < (1 << depth))

  // d = depth (specificity) of the node at the address
  def toAddress(d: Int): QuadAddr = {
    require(isValid)
    require(d >= simplify.depth, s"$this is more specific than depth $d")
    var currentX = x
    var currentY = y
    var addr = new QuadAddr()
    for (currentDepth <- 0 until d) {
      val gridDepth = depth - 1 - currentDepth
      // calculate grid size from depth
      val currentGridSize = if (gridDepth >= 0) {
          1 << gridDepth
        } else {
          0
        }
      // find which quadrant we are in, and subtract from currentX/Y
      val quadrant = if (gridDepth >= 0) {
          new Quadrant(currentX >= currentGridSize, currentY >= currentGridSize)
        } else {
          Quadrant.BottomLeft
        }
      currentX = currentX - (if (quadrant.x) currentGridSize else 0)
      currentY = currentY - (if (quadrant.y) currentGridSize else 0)
      addr = addr + quadrant
    }
    addr
  }

  // Operators

  def +(other: QuadOffset): QuadOffset = {
    QuadOffset.normalize(this, other) match {
      case (maxDepth, normed, otherNormed) =>
        new QuadOffset(maxDepth,
          normed.x + otherNormed.x,
          normed.y + otherNormed.y).simplify
    }
  }

  def -(other: QuadOffset): QuadOffset = {
    QuadOffset.normalize(this, other) match {
      case (maxDepth, normed, otherNormed) =>
        new QuadOffset(maxDepth,
          normed.x - otherNormed.x,
          normed.y - otherNormed.y).simplify
    }
  }

  def *(k: Int): QuadOffset = new QuadOffset(depth, x*k, y*k).simplify

  // Shift operators are used for multiplying the offset by a power of 2,
  //   effectively changing its depth. This is used to handle QuadTrees of
  //   different sizes.
  def <<(levels: Int): QuadOffset =
      new QuadOffset(depth - levels, x, y).simplify
  def >>(levels: Int): QuadOffset =
      new QuadOffset(depth + levels, x, y).simplify

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
    case other: QuadOffset =>
        depth == other.depth &&
        x == other.x &&
        y == other.y
    case _ => false
  }

  override def toString: String = s"QuadOffset($x, $y) / 2^$depth"
}
