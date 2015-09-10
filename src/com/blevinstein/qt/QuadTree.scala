package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

object QuadTree {
  // returns a mapping from the unit Rectangle to the given quadrant
  def zoomFunc(quad : Quadrant): (Point => Point) =
    (p) => (p + new Point(if (quad.x) 1 else 0, if (quad.y) 1 else 0)) / 2

  // returns an approximation of the f over the unit Rectangle up to a maximum
  // depth
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

  type Operator = (Material, Material) => Material

  // Used for constructing operators on QuadTrees
  def apply(op: Operator)(q1: QuadTree, q2: QuadTree): QuadTree = {
    def applyBranchLeaf(branch: QuadBranch, leaf: QuadLeaf): QuadTree = {
      new QuadBranch(
        apply(op)(branch.a, leaf),
        apply(op)(branch.b, leaf),
        apply(op)(branch.c, leaf),
        apply(op)(branch.d, leaf))
        .tryMerge
    }
    (q1, q2) match {
      case (b1: QuadBranch, b2: QuadBranch) =>
        new QuadBranch(
          apply(op)(b1.a, b2.a),
          apply(op)(b1.b, b2.b),
          apply(op)(b1.c, b2.c),
          apply(op)(b1.d, b2.d))
          .tryMerge
      case (branch: QuadBranch, leaf: QuadLeaf) => applyBranchLeaf(branch, leaf)
      case (leaf: QuadLeaf, branch: QuadBranch) => applyBranchLeaf(branch, leaf)
      case (l1: QuadLeaf, l2: QuadLeaf) =>
        new QuadLeaf(op(l1.material, l2.material))
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
      def build_recur(addr: QuadAddr): QuadTree = {
        val exactPieces = pieces.filter(_._1 == addr)
        if (!exactPieces.isEmpty) {
          new QuadLeaf(exactPieces.head._2 /* mat */)
        } else {
          val smallerPieces = pieces.filter(_._1 isInside addr)
          if (!smallerPieces.isEmpty) {
            new QuadBranch(build_recur(addr + Quadrant.TopLeft),
              build_recur(addr + Quadrant.TopRight),
              build_recur(addr + Quadrant.BottomLeft),
              build_recur(addr + Quadrant.BottomRight))
              .tryMerge
          } else {
            new QuadLeaf(background)
          }
        }
      }
      build_recur(new QuadAddr())
    }
  }
}

abstract class QuadTree {
  def getMaterial(p: Point): Material = {
    require(p.x >= 0 && p.x <= 1)
    require(p.y >= 0 && p.y <= 1)
    this match {
      case branch: QuadBranch =>
        branch.getSubtree(Quadrant.of(p)).getMaterial((p * 2) % 1)
      case leaf: QuadLeaf => leaf.material
    }
  }

  // TODO: add getRectangle(QuadAddr) or equivalent?
  def getMaterial(addr: QuadAddr): Material = this match {
    case branch: QuadBranch =>
      branch.getSubtree(addr.head).getMaterial(addr.tail)
    case leaf: QuadLeaf => leaf.material
  }

  // TODO: refactor recursive function inside iter
  type IterCallback = (Rectangle, Material) => Unit
  def iter(cb: IterCallback): Unit = {
    def iter_recur(cb: IterCallback, qt: QuadTree, rect: Rectangle): Unit = {
      qt match {
        case branch: QuadBranch => Quadrant.values.foreach((quadrant) =>
            iter_recur(cb,
              branch.getSubtree(quadrant),
              rect.getQuadrant(quadrant)))
          case leaf: QuadLeaf => cb(rect, leaf.material)
      }
    }
    iter_recur(cb, this, Rectangle.unit)
  }

  override def toString: String = {
    this match {
      case branch: QuadBranch =>
        s"[[${branch.a},${branch.b}][${branch.c},${branch.d}]]"
      case leaf: QuadLeaf => s"${leaf.material}"
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

