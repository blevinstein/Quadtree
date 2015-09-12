package com.blevinstein.qt

class QuadAddr(val quads: List[Quadrant]) {
  def this(someQuads: Quadrant*) = this(someQuads.toList)
  // copy constructor
  def this(addr: QuadAddr) = this(addr.quads)

  def toOffset: QuadOffset = {
    var bottomLeft = QuadOffset.zero
    for (i <- 0 until this.length) {
      val quadrant = this.apply(i)
      bottomLeft = bottomLeft + new QuadOffset(i + 1,
        if (quadrant.x) 1 else 0,
        if (quadrant.y) 1 else 0)
    }
    bottomLeft
  }

  // delegate to quads
  def apply(idx: Int): Quadrant = quads.apply(idx)
  def iterator: Iterator[Quadrant] = quads.iterator
  def length: Int = quads.length
  def head: Quadrant = quads.head
  def tail: QuadAddr = new QuadAddr(quads.tail)

  // (A inside B) iff (B isPrefixOf A)
  def isInside(other: QuadAddr): Boolean = quads startsWith other.quads

  // scalastyle:off method.name spaces.after.plus
  def +(other: QuadAddr): QuadAddr = new QuadAddr(quads ++ other.quads)
  def +(quad: Quadrant): QuadAddr = new QuadAddr(quads :+ quad)

  override def hashCode: Int = quads.hashCode
  override def equals(o: Any): Boolean = o match {
    case other: QuadAddr => quads == other.quads
    case _ => false
  }

  override def toString: String = s"QuadAddr($quads)"
}

