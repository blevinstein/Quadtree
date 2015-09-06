package com.blevinstein.qt

// [ a b ]
// [ c d ]

abstract class QuadTree {
  def get(p: Point): Material = {
    require(p.x >= 0 && p.x <= 1)
    require(p.y >= 0 && p.y <= 1)
    this match {
      case branch: QuadBranch =>
        if (p.x < 0.5)
          if (p.y < 0.5)
            branch.a.get(new Point(2 * p.x, 2 * p.y))
          else
            branch.c.get(new Point(2 * p.x, 2 * p.y - 1))
        else
          if (p.y < 0.5)
            branch.b.get(new Point(2 * p.x - 1, 2 * p.y))
          else
            branch.d.get(new Point(2 * p.x - 1, 2 * p.y - 1))
      case leaf: QuadLeaf => leaf.material
    }
  }

  def iter(cb: (Rectangle, Material) => Unit): Unit = {
    iter(cb, new Rectangle(Point.ZERO, new Point(1, 1)))
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
class QuadBranch(val a: QuadTree, val b: QuadTree,
  val c: QuadTree, val d: QuadTree) extends QuadTree
class QuadLeaf(val material: Material) extends QuadTree

object Point {
  val ZERO = new Point(0, 0)
}
class Point(val x: Float, val y: Float) {
  def +(other: Point): Point = new Point(x + other.x, y + other.y)
  def -(other: Point): Point = new Point(x - other.x, y - other.y)
  def *(k: Float): Point = new Point(x * k, y * k)
  def /(k: Float): Point = new Point(x / k, y / k)
}
class Rectangle(val min: Point, val max: Point) {
  val center = (min + max) / 2
  def contains(p: Point) = {
    p.x > min.x && p.x < max.x && p.y > min.y && p.y < max.y
  }
  def topLeft = new Rectangle(min, center)
  def topRight =
    new Rectangle(new Point(center.x, min.y), new Point(max.x, center.y))
  def bottomLeft =
    new Rectangle(new Point(min.x, center.y), new Point(center.x, max.y))
  def bottomRight = new Rectangle(center, max)
}

object Material {
  val EMPTY = new Material
  val FULL = new Material
}

class Material {
}
