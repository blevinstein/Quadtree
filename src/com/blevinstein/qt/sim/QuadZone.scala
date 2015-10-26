package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadLen,QuadOffset,QuadRectangle}

object QuadZone {
  val size = QuadOffset.one

  def around(offset: QuadOffset): QuadZone =
      new QuadZone((offset.x.toFloat % 1).toInt, (offset.y.toFloat % 1).toInt)

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
  val min: QuadOffset = new QuadOffset(new QuadLen(x, 0), new QuadLen(y, 0))

  val toQuadRectangle: QuadRectangle =
      new QuadRectangle(min, min + QuadZone.size)

  override def toString: String = s"QuadZone($x, $y)"
}
