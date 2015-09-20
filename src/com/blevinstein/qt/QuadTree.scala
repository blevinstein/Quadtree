package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

object QuadTree {
  // returns a mapping from the unit rectangle to the given quadrant
  def zoomFunc(quad : Quadrant): (Point => Point) =
    (p) => (p + new Point(if (quad.x) 1 else 0, if (quad.y) 1 else 0)) / 2

  // returns an approximation of the f over the unit rectangle up to a maximum
  // depth
  def approx[T](depth: Int, f: Point => T): QuadTree[T] = {
    if (depth <= 0) {
      new QuadLeaf(f(new Point(0.5f, 0.5f)))
    } else {
      QuadBranch.create(
        (quadrant) => approx(depth - 1, f compose QuadTree.zoomFunc(quadrant)))
        .tryMerge
    }
  }

  type Operator[T] = (T, T) => T

  // Used for constructing operators on QuadTrees
  // TODO: handle offset: QuadOffset of q2 with respect to q1
  // TODO: handle differently-sized QuadTrees? 'depth offset'?
  def merge[T](op: Operator[T])(q1: QuadTree[T],
      q2: QuadTree[T]): QuadTree[T] = {
    def mergeBranchLeaf(branch: QuadBranch[T],
        leaf: QuadLeaf[T]): QuadTree[T] = {
      branch.map((tree, quadrant) => merge(op)(tree, leaf)).tryMerge
    }
    (q1, q2) match {
      case (b1: QuadBranch[T], b2: QuadBranch[T]) =>
        b1.map((tree, quadrant) => merge(op)(tree, b2.getSubtree(quadrant)))
          .tryMerge
      case (branch: QuadBranch[T], leaf: QuadLeaf[T]) => mergeBranchLeaf(branch, leaf)
      case (leaf: QuadLeaf[T], branch: QuadBranch[T]) => mergeBranchLeaf(branch, leaf)
      case (l1: QuadLeaf[T], l2: QuadLeaf[T]) =>
        new QuadLeaf(op(l1.data, l2.data))
    }
  }

  class Builder[T](background: T) {
    var pieces: List[(QuadAddr, T)] = List()

    def add(addr: QuadAddr, data: T): Builder[T] = {
      pieces = pieces :+ ((addr, data))
      this
    }

    def build: QuadTree[T] = {
      def build_recur(addr: QuadAddr): QuadTree[T] = {
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

abstract class QuadTree[T] {
  def getData(p: Point): T = {
    require(p.x >= 0 && p.x <= 1)
    require(p.y >= 0 && p.y <= 1)
    this match {
      case branch: QuadBranch[T] =>
        branch.getSubtree(Quadrant.of(p)).getData((p * 2) % 1)
      case leaf: QuadLeaf[T] => leaf.data
    }
  }

  // TODO: return List[T]
  def getData(addr: QuadAddr): T = this match {
    case branch: QuadBranch[T] =>
      branch.getSubtree(addr.head).getData(addr.tail)
    case leaf: QuadLeaf[T] => leaf.data
  }

  // Maximum depth of this QuadTree.
  def maxDepth: Int = this match {
    case branch: QuadBranch[T] => List(branch.a.maxDepth + 1,
      branch.b.maxDepth + 1,
      branch.c.maxDepth + 1,
      branch.d.maxDepth + 1).max
    case leaf: QuadLeaf[T] => 0
  }

  // TODO: Implement prune(depth: Int): QuadTree such that r.maxDepth == depth
  // PROBEM: need a way of merging? can take merge function of type
  //   (Material x 4) => Material, e.g. "mode", but sampling must be recursive?

  /**
   * For each QuadLeaf in this QuadTree, emits its address and data
   */
  type IterCallback[T] = (QuadAddr, T) => Unit
  def iter(cb: IterCallback[T]): Unit = {
    def iter_recur(cb: IterCallback[T], qt: QuadTree[T],
        addr: QuadAddr): Unit = {
      qt match {
        case branch: QuadBranch[T] => Quadrant.values.foreach((quadrant) =>
            iter_recur(cb,
              branch.getSubtree(quadrant),
              addr + quadrant))
          case leaf: QuadLeaf[T] => cb(addr, leaf.data)
      }
    }
    iter_recur(cb, this, new QuadAddr())
  }

  override def toString: String = {
    this match {
      case branch: QuadBranch[T] =>
        s"[[${branch.a},${branch.b}][${branch.c},${branch.d}]]"
      case leaf: QuadLeaf[T] => s"${leaf.data}"
    }
  }
}

object QuadBranch {
  def create[T](f: Quadrant => QuadTree[T]): QuadBranch[T] = new QuadBranch(
    f(TopLeft), f(TopRight), f(BottomLeft), f(BottomRight))
}
class QuadBranch[T](val a: QuadTree[T],
    val b: QuadTree[T],
    val c: QuadTree[T],
    val d: QuadTree[T]) extends QuadTree[T] {
  def getSubtree(quadrant : Quadrant): QuadTree[T] = quadrant match {
    case TopLeft => a
    case TopRight => b
    case BottomLeft => c
    case BottomRight => d
  }

  /**
   * If this is a QuadBranch with 4 identical QuadLeafs, merge into one QuadLeaf
   * Else, return this
   */
  def tryMerge: QuadTree[T] = {
    a match {
      case leaf: QuadLeaf[T] => if (a == b && a == c && a == d) {
        new QuadLeaf(leaf.data)
      } else {
        this // no merge
      }
      case _ => this // no merge
    }
  }

  def map(f: (QuadTree[T], Quadrant) => QuadTree[T]): QuadBranch[T] =
      new QuadBranch(f(a, TopLeft),
        f(b, TopRight),
        f(c, BottomLeft),
        f(d, BottomRight))

  override def hashCode: Int =
    31 * (a.hashCode +
      31 * (b.hashCode +
        31 * (c.hashCode +
          31 * d.hashCode)))

  override def equals(o: Any): Boolean = o match {
    case other: QuadBranch[T] => a == other.a && b == other.b &&
        c == other.c && d == other.d
    case _ => false
  }
}

class QuadLeaf[T](val data: T) extends QuadTree[T] {
  override def hashCode: Int = data.hashCode

  override def equals(o: Any): Boolean = o match {
      case other: QuadLeaf[T] => data == other.data
      case _ => false
  }
}

