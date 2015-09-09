package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

object QuadTree {
  // TODO: if necessary, refactor into a real class (and separate file)
  type QuadAddr = List[Quadrant]

  def zoomFunc(quad : Quadrant): (Point => Point) =
    (p) => (p + new Point(if (quad.x) 1 else 0, if (quad.y) 1 else 0)) / 2

  def approx(depth: Int, f: Point => Material): QuadTree = {
    if (depth <= 0) {
      new QuadLeaf(f(new Point(0.5f, 0.5f)))
    } else {
      new QuadBranch(
        approx(depth - 1, f compose QuadTree.zoomFunc(TopLeft)),
        approx(depth - 1, f compose QuadTree.zoomFunc(TopRight)),
        approx(depth - 1, f compose QuadTree.zoomFunc(BottomLeft)),
        approx(depth - 1, f compose QuadTree.zoomFunc(BottomRight)))
        .tryMerge
    }
  }

  class Builder(background: Material = Material.Empty) {
    var pieces: List[(QuadAddr, Material)] = List()

    def add(addr: QuadAddr, mat: Material): Builder = {
      pieces = pieces :+ ((addr, mat))
      this
    }

    def build: QuadTree = {
      // scalastyle:off method.name
      def build_recur(prefix: QuadAddr): QuadTree = {
        val exactPieces = pieces.filter(_._1 /* addr */ == prefix)
        if (!exactPieces.isEmpty) {
          new QuadLeaf(exactPieces.head._2 /* mat */)
        } else {
          val smallerPieces = pieces.filter(_._1 /* addr */ startsWith prefix)
          if (!smallerPieces.isEmpty) {
            new QuadBranch(build_recur(prefix :+ Quadrant.TopLeft),
              build_recur(prefix :+ Quadrant.TopRight),
              build_recur(prefix :+ Quadrant.BottomLeft),
              build_recur(prefix :+ Quadrant.BottomRight))
              .tryMerge
          } else {
            new QuadLeaf(background)
          }
        }
      }
      build_recur(List())
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

class QuadBranch(val a: QuadTree,
    val b: QuadTree,
    val c: QuadTree,
    val d: QuadTree) extends QuadTree {
  def getSubtree(quadrant : Quadrant): QuadTree = quadrant match {
    case TopLeft => a
    case TopRight => b
    case BottomLeft => c
    case BottomRight => d
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

  override def hashCode: Int =
    31 * (a.hashCode +
      31 * (b.hashCode +
        31 * (c.hashCode +
          31 * d.hashCode)))

  override def equals(o: Any): Boolean = o match {
    case other: QuadBranch => a == other.a && b == other.b &&
        c == other.c && d == other.d
    case _ => false
  }
}

class QuadLeaf(val material: Material) extends QuadTree {
  override def hashCode: Int = material.hashCode

  override def equals(o: Any): Boolean = o match {
      case other: QuadLeaf => material == other.material
      case _ => false
  }
}

