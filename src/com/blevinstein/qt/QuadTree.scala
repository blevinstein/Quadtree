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

  // Used for constructing operators on QuadTrees
  def merge[X,Y,Z](op: (X, Y) => Z)(q1: QuadTree[X],
      q2: QuadTree[Y]): QuadTree[Z] = {
    (q1, q2) match {
      case (b1: QuadBranch[X], b2: QuadBranch[Y]) =>
        b1.map((tree, quadrant) => merge(op)(tree, b2.getSubtree(quadrant)))
          .tryMerge
      case (branch: QuadBranch[X], leaf: QuadLeaf[Y]) =>
        branch.map((subtree, quadrant) => merge(op)(subtree, leaf)).tryMerge
      case (leaf: QuadLeaf[X], branch: QuadBranch[Y]) =>
        branch.map((subtree, quadrant) => merge(op)(leaf, subtree)).tryMerge
      case (l1: QuadLeaf[X], l2: QuadLeaf[Y]) =>
        new QuadLeaf(op(l1.data, l2.data))
    }
  }

  def transform[X,Y](op: X => Y)(arg: QuadTree[X]): QuadTree[Y] = arg match {
    case branch: QuadBranch[X] =>
      branch.map((tree, _) => transform(op)(tree)).tryMerge
    case leaf: QuadLeaf[X] => new QuadLeaf(op(leaf.data))
  }

  def reduce[X](op: List[X] => X)(arg: QuadTree[X]): X = arg match {
    case branch: QuadBranch[X] => op(branch.subtrees.map(reduce(op)(_)))
    case leaf: QuadLeaf[X] => leaf.data
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

abstract class QuadTree[+T] {
  def getData(p: Point): T = {
    require(p.x >= 0 && p.x <= 1)
    require(p.y >= 0 && p.y <= 1)
    this match {
      case branch: QuadBranch[T] =>
        branch.getSubtree(Quadrant.of(p)).getData((p * 2) % 1)
      case leaf: QuadLeaf[T] => leaf.data
    }
  }

  def getData(addr: QuadAddr): T = this match {
    case branch: QuadBranch[T] =>
      branch.getSubtree(addr.head).getData(addr.tail)
    case leaf: QuadLeaf[T] => leaf.data
  }

  // Maximum depth of this QuadTree.
  val maxDepth: Int = this match {
    case branch: QuadBranch[T] => List(branch.a.maxDepth + 1,
      branch.b.maxDepth + 1,
      branch.c.maxDepth + 1,
      branch.d.maxDepth + 1).max
    case leaf: QuadLeaf[T] => 0
  }

  // TODO: Implement prune(depth: Int): QuadTree such that r.maxDepth == depth
  // use QuadTree.reduce to simplify below that depth

  /**
   * For each QuadLeaf in this QuadTree, emits its address and data
   */
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

  def grow[T2 >: T](levels: Int, offset: QuadOffset, bg: T2): QuadTree[T2] = {
    val newMaxDepth = maxDepth - levels
    val scanSize = 1 << maxDepth
    val builder = new QuadTree.Builder[T2](bg)
    for (i <- 0 until scanSize) {
      for (j <- 0 until scanSize) {
        val fromAddr = new QuadOffset(maxDepth, i, j).toAddress(maxDepth)
        val toOffset = new QuadOffset(newMaxDepth, i, j) + offset
        if (toOffset.isValid) {
          val toAddr = toOffset.toAddress(newMaxDepth)
          builder.add(toAddr, getData(fromAddr))
        } // else toAddr would be out of bounds
      }
    }
    builder.build
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
/**
 * +y
 * ^^
 * [ a b ]
 * [ c d ] -> +x
 */
class QuadBranch[T](val a: QuadTree[T],
    val b: QuadTree[T],
    val c: QuadTree[T],
    val d: QuadTree[T]) extends QuadTree[T] {

  val subtrees = List(a, b, c, d)

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

  def map[O](f: (QuadTree[T], Quadrant) => QuadTree[O]): QuadBranch[O] =
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

