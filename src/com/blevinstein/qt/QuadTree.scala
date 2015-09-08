package com.blevinstein.qt

// [ a b ]
// [ c d ]

abstract class QuadTree {
  def get(p: Point): Material = {
    require(p.x >= 0 && p.x <= 1)
    require(p.y >= 0 && p.y <= 1)
    this match {
      case branch: QuadBranch =>
        if (p.x < 0.5) {
          if (p.y < 0.5) {
            branch.a.get(new Point(2 * p.x, 2 * p.y))
          } else {
            branch.c.get(new Point(2 * p.x, 2 * p.y - 1))
          }
        } else {
          if (p.y < 0.5) {
            branch.b.get(new Point(2 * p.x - 1, 2 * p.y))
          } else {
            branch.d.get(new Point(2 * p.x - 1, 2 * p.y - 1))
          }
        }
      case leaf: QuadLeaf => leaf.material
    }
  }

  def iter(cb: (Rectangle, Material) => Unit): Unit = {
    iter(cb, Rectangle.unit)
  }
  def iter(cb: (Rectangle, Material) => Unit, rect: Rectangle): Unit = {
    this match {
      case branch: QuadBranch => {
        branch.a.iter(cb, rect.topLeft)
        branch.b.iter(cb, rect.topRight)
        branch.c.iter(cb, rect.bottomLeft)
        branch.d.iter(cb, rect.bottomRight)
      }
      case leaf: QuadLeaf => cb(rect, leaf.material)
    }
  }
}
class QuadBranch(val a: QuadTree,
  val b: QuadTree,
  val c: QuadTree,
  val d: QuadTree) extends QuadTree
class QuadLeaf(val material: Material) extends QuadTree
