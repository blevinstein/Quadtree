package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadTree,QuadOffset,QuadRectangle}

class QuadObject(val position: QuadRectangle,
    val shape: QuadTree[Option[Material]]) {

  require(position.isPerfectSquare)

  def +(offset: QuadOffset): QuadObject =
      new QuadObject(position + offset, shape)

  val toQuadTree: QuadTree[Option[Material]] =
      shape.grow(position.perfectLog.get, position.min, Material.Empty)
}
