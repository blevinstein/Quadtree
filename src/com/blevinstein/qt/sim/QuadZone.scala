package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadLen,QuadOffset,QuadRectangle}

object QuadZone {
  val size = QuadOffset.one

  def around(offset: QuadOffset): QuadZone =
      new QuadZone(offset.x.toFloat.toInt, offset.y.toFloat.toInt)

  def around(rect: QuadRectangle): List[QuadZone] = {
    var zones = List[QuadZone]()
    var minZone = around(rect.min)
    var maxZone = around(rect.max)
    for (i <- minZone.x until maxZone.x + 1) {
      for (j <- minZone.y until maxZone.y + 1) {
        zones = new QuadZone(i, j) :: zones
      }
    }
    zones
  }
}
class QuadZone(val x: Int, val y: Int) {
  val min: QuadOffset = new QuadOffset(QuadLen(x, 0), QuadLen(y, 0))

  val toQuadRectangle: QuadRectangle =
      new QuadRectangle(min, min + QuadZone.size)

  override def hashCode: Int =
      31 * (x.hashCode +
          31 * y.hashCode)

  override def equals(o: Any): Boolean = o match {
    case other: QuadZone => x == other.x && y == other.y
    case _ => false
  }

  override def toString: String = s"QuadZone($x, $y)"
}
