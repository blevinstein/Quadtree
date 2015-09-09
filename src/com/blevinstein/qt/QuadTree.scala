package com.blevinstein.qt

object QuadTree {
  def zoomFunc(quad : Quadrant): (Point => Point) =
    (p) => (p + new Point(if (quad.x) 1 else 0, if (quad.y) 1 else 0)) / 2
  def approx(depth: Int, f: Point => Material): QuadTree = {
    if (depth <= 0) {
      new QuadLeaf(f(new Point(0.5f, 0.5f)))
    } else {
      new QuadBranch(
        approx(depth - 1, f compose QuadTree.zoomFunc(Quadrant.TopLeft)),
        approx(depth - 1, f compose QuadTree.zoomFunc(Quadrant.TopRight)),
        approx(depth - 1, f compose QuadTree.zoomFunc(Quadrant.BottomLeft)),
        approx(depth - 1, f compose QuadTree.zoomFunc(Quadrant.BottomRight)))
        .tryMerge
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
          .append(branch.getSubtree(Quadrant.TopLeft)).append(",")
          .append(branch.getSubtree(Quadrant.TopRight)).append("][")
          .append(branch.getSubtree(Quadrant.BottomLeft)).append(",")
          .append(branch.getSubtree(Quadrant.BottomRight)).append("]]")
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
    case Quadrant.TopLeft => a
    case Quadrant.TopRight => b
    case Quadrant.BottomLeft => c
    case Quadrant.BottomRight => d
  }
  def tryMerge: QuadTree = {
    a match {
      case leaf: QuadLeaf => if (a == b && a == c && a == d) {
        new QuadLeaf(leaf.material)
      } else {
        this // no merge
      }
      case _ => this // no merge
    }
  }
}

class QuadLeaf(val material: Material) extends QuadTree {
  override def hashCode: Int = material.hashCode

  override def equals(o: Any): Boolean = {
    o match {
      case other: QuadLeaf => material == other.material
      case _ => false
    }
  }
}

