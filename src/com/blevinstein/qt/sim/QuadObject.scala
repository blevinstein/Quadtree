package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadTree,QuadOffset}

class QuadObject(val size: Int,
    val position: QuadOffset,
    val shape: QuadTree[Option[Material]]) {

  def +(offset: QuadOffset): QuadObject =
      new QuadObject(size, position + offset, shape)

  def toQuadTree: QuadTree[Option[Material]] =
      shape.grow(size, position, Material.Empty)
}
