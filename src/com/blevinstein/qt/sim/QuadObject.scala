package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadTree,QuadOffset}

class QuadObject(val size: Int,
    val position: QuadOffset,
    val shape: QuadTree[Option[Material]]) {
  def toQuadTree: QuadTree[Option[Material]] =
      shape.grow(size, position, Material.Empty)
}
