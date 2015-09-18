package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

object QuadTree {
  // returns a mapping from the unit rectangle to the given quadrant
  def zoomFunc(quad : Quadrant): (Point => Point) =
    (p) => (p + new Point(if (quad.x) 1 else 0, if (quad.y) 1 else 0)) / 2

  // returns an approximation of the f over the unit rectangle up to a maximum
  // depth
  def approx(depth: Int, f: Point => Material): QuadTree = {
    if (depth <= 0) {
      new QuadLeaf(f(new Point(0.5f, 0.5f)))
    } else {
      QuadBranch.create(
        (quadrant) => approx(depth - 1, f compose QuadTree.zoomFunc(quadrant)))
        .tryMerge
    }
  }

  type Operator = (Material, Material) => Material

  // Used for constructing operators on QuadTrees
  def merge(op: Operator)(q1: QuadTree, q2: QuadTree): QuadTree = {
    def mergeBranchLeaf(branch: QuadBranch, leaf: QuadLeaf): QuadTree = {
      branch.map((tree, quadrant) => merge(op)(tree, leaf)).tryMerge
    }
    (q1, q2) match {
      case (b1: QuadBranch, b2: QuadBranch) =>
        b1.map((tree, quadrant) => merge(op)(tree, b2.getSubtree(quadrant)))
          .tryMerge
      case (branch: QuadBranch, leaf: QuadLeaf) => mergeBranchLeaf(branch, leaf)
      case (leaf: QuadLeaf, branch: QuadBranch) => mergeBranchLeaf(branch, leaf)
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
      def build_recur(addr: QuadAddr): QuadTree = {
        val exactPieces = pieces.filter(_._1 == addr)
        if (!exactPieces.isEmpty) {
          new QuadLeaf(exactPieces.head._2 /* mat */)
        } else {
          val smallerPieces = pieces.filter(_._1 isInside addr)
          if (!smallerPieces.isEmpty) {
            QuadBranch.create((quadrant) =>
                build_recur(addr + quadrant)).tryMerge
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

  // TODO: return List[Material]
  def getMaterial(addr: QuadAddr): Material = this match {
    case branch: QuadBranch =>
      branch.getSubtree(addr.head).getMaterial(addr.tail)
    case leaf: QuadLeaf => leaf.material
  }

  // Maximum depth of this QuadTree.
  def maxDepth: Int = this match {
    case branch: QuadBranch => List(branch.a.maxDepth + 1,
      branch.b.maxDepth + 1,
      branch.c.maxDepth + 1,
      branch.d.maxDepth + 1).max
    case leaf: QuadLeaf => 0
  }

  // TODO: Implement prune(depth: Int): QuadTree such that r.maxDepth == depth
  // PROBEM: need a way of merging materials? can take merge function of type
  //   (Material x 4) => Material, e.g. "mode", but sampling must be recursive?

  /**
   * For each QuadLeaf in this QuadTree, emits its address and material
   */
  type IterCallback = (QuadAddr, Material) => Unit
  def iter(cb: IterCallback): Unit = {
    def iter_recur(cb: IterCallback, qt: QuadTree, addr: QuadAddr): Unit = {
      qt match {
        case branch: QuadBranch => Quadrant.values.foreach((quadrant) =>
            iter_recur(cb,
              branch.getSubtree(quadrant),
              addr + quadrant))
          case leaf: QuadLeaf => cb(addr, leaf.material)
      }
    }
    iter_recur(cb, this, new QuadAddr())
  }

  override def toString: String = {
    this match {
      case branch: QuadBranch =>
        s"[[${branch.a},${branch.b}][${branch.c},${branch.d}]]"
      case leaf: QuadLeaf => s"${leaf.material}"
    }
  }
}

object QuadBranch {
  def create(f: Quadrant => QuadTree): QuadBranch = new QuadBranch(
    f(TopLeft), f(TopRight), f(BottomLeft), f(BottomRight))
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

  /**
   * If this is a QuadBranch with 4 identical QuadLeafs, merge into one QuadLeaf
   * Else, return this
   */
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

  def map(f: (QuadTree, Quadrant) => QuadTree): QuadBranch = new QuadBranch(
      f(a, TopLeft),
      f(b, TopRight),
      f(c, BottomLeft),
      f(d, BottomRight))

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

