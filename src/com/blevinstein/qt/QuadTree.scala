package com.blevinstein.qt

import com.blevinstein.qt.Quadrant._

abstract class QuadTree {
  def get(p: Point): Material = {
    require(p.x >= 0 && p.x <= 1)
    require(p.y >= 0 && p.y <= 1)
    this match {
      case branch: QuadBranch =>
        branch.getSubtree(Quadrant.of(p)).get((p * 2) % 1)
      case leaf: QuadLeaf => leaf.material
    }
  }

  def iter(cb: (Rectangle, Material) => Unit): Unit = {
    iter(cb, Rectangle.unit)
  }
  def iter(cb: (Rectangle, Material) => Unit, rect: Rectangle): Unit = {
    this match {
      case branch: QuadBranch => Quadrant.values.foreach((quadrant) =>
          branch.getSubtree(quadrant).iter(cb, rect.getQuadrant(quadrant)))
      case leaf: QuadLeaf => cb(rect, leaf.material)
    }
  }
}
class QuadBranch(a: QuadTree,
    b: QuadTree,
    c: QuadTree,
    d: QuadTree) extends QuadTree {
  def getSubtree(quadrant : Quadrant): QuadTree = quadrant match {
    case TopLeft => a
    case TopRight => b
    case BottomLeft => c
    case BottomRight => d
  }
}
class QuadLeaf(val material: Material) extends QuadTree
