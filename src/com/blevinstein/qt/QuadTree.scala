package com.blevinstein.qt

// scalastyle:off underscore.import
import com.blevinstein.qt.Quadrant._

object QuadTree {
  // TODO: implement merging of redundant QuadBranches
  def approx(depth: Int, f: Point => Material): QuadTree = {
    if (depth <= 0) {
      new QuadLeaf(f(new Point(0.5f, 0.5f)))
    } else {
      new QuadBranch(approx(depth - 1, f compose Point.zoomFunc(TopLeft)),
        approx(depth - 1, f compose Point.zoomFunc(TopRight)),
        approx(depth - 1, f compose Point.zoomFunc(BottomLeft)),
        approx(depth - 1, f compose Point.zoomFunc(BottomRight)))
    }
  }
}
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

  override def toString: String = {
    this match {
      case branch: QuadBranch => new StringBuilder("[[")
          .append(branch.getSubtree(TopLeft)).append(",")
          .append(branch.getSubtree(TopRight)).append("][")
          .append(branch.getSubtree(BottomLeft)).append(",")
          .append(branch.getSubtree(BottomRight)).append("]]")
          .toString
      case leaf: QuadLeaf => leaf.material.toString
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

