package com.blevinstein.qt

import com.blevinstein.geom.{Point}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}
import com.blevinstein.util.Search

// This is the heart of the quad engine. A QuadTree can be used to store
// arbitrary data in a quadtree structure.
//
// QuadTrees can be constructed from QuadLeaf and QuadBranch nodes.
//
// QuadTrees can be merged together using [merge], evaluated into a single
// element using [reduce], and have simple, local transformations (within each
// leaf node) using [transform].
//
// QuadTrees can contain complex types, so you can use complex types like:
// - QuadTree[Option[T]], with different treatment of empty leaf nodes
// - QuadTree[X => Y], used to perform complex transformations of a QuadTree
object QuadTree {
  // Returns a mapping from the unit rectangle to the given quadrant.
  def zoomFunc(quad: Quadrant): (Point => Point) =
      (p) => (p + new Point(if (quad.x) 1 else 0, if (quad.y) 1 else 0)) / 2

  // Returns an approximation of the f over the unit rectangle up to a maximum
  // depth.
  def approx[T](depth: Int, f: Point => T): QuadTree[T] = {
    if (depth <= 0) {
      new QuadLeaf(f(new Point(0.5f, 0.5f)))
    } else {
      QuadBranch.create(
        (quadrant) => approx(depth - 1, f compose QuadTree.zoomFunc(quadrant)))
        .tryMerge
    }
  }

  // Used for constructing operators on QuadTrees: [merge], [transform],
  // [reduce]

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

  // Used for constructing a QuadTree, by QuadAddr or QuadRectangle
  class Builder[T](background: T) {
    var pieces: List[(QuadAddr, T)] = List()

    def add(addr: QuadAddr, data: T): Builder[T] = {
      pieces = (addr, data) ::
          pieces.filter { case (oldAddr, data) => !oldAddr.isInside(addr) }
      this
    }

    def addAll(rect: QuadRectangle, data: T): Builder[T] = {
      for (addr <- rect.toAddressList) {
        add(addr, data)
      }
      this
    }

    def addAll(addrs: Iterable[QuadAddr], data: T): Builder[T] = {
      for (addr <- addrs) {
        add(addr, data)
      }
      this
    }

    def addAllRects(rects: Iterable[QuadRectangle], data: T): Builder[T] = {
      for (rect <- rects) {
        addAll(rect, data)
      }
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
// Implemented by QuadBranch[T] and QuadLeaf[T].
abstract class QuadTree[+T] {
  def toBuilder[T1 >: T]: QuadTree.Builder[T1] = {
    // NOTE: Code smell here. Background argument wants to be omitted, because
    // the unit rectangle is entirely covered by the leaves of this tree.
    val someData = getData(getAddr(Point.zero))

    val builder = new QuadTree.Builder[T1](someData)
    iter((addr, data) => builder.add(addr, data))
    builder
  }

  // Returns the address of the leaf node that contains this point
  def getAddr(p: Point): QuadAddr = {
    require(p.x >= 0 && p.x <= 1)
    require(p.y >= 0 && p.y <= 1)
    this match {
      case branch: QuadBranch[T] => {
        val quadrant = Quadrant.of(p)
        new QuadAddr(
          quadrant :: branch.getSubtree(quadrant).getAddr((p * 2) % 1))
      }
      case leaf: QuadLeaf[T] => new QuadAddr()
    }
  }

  def getData(addr: QuadAddr): T = this match {
    case branch: QuadBranch[T] => {
      require(!addr.isEmpty)
      branch.getSubtree(addr.head).getData(addr.tail)
    }
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

  // For each QuadLeaf in this QuadTree, emits its address and data
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

  // Resizes this quadtree by scaling up by 2^[levels] and translating [offset].
  // Any area of the output not covered by the input tree will be filled with
  // [fill].
  // TODO: think of a better name for this
  def grow[T1 >: T](levels: Int, offset: QuadOffset, fill: T1): QuadTree[T1] = {
    val builder = new QuadTree.Builder[T1](fill)
    iter((addr, data) => {
      val quadRect = (addr.toQuadRectangle << levels) + offset
      builder.addAll(quadRect, getData(addr))
    })
    builder.build
  }

  // Returns a list of continuous regions by material
  def getRegions: List[(T, List[QuadAddr])] = {
    val graph = getGraph
    var regions = List[(T, List[QuadAddr])]()
    var visited = Set[QuadAddr]()

    for (current <- graph.keys) {
      val data = getData(current)
      if (!visited.contains(current)) {
        val region = Search.floodfill(current,
            (a: QuadAddr) => graph.get(a).get.filter((b) => getData(b) == data))
        visited ++= region.toSet
        regions = (data, region) :: regions
      }
    }

    regions
  }

  // Calculates a graph representation of this QuadTree, where each QuadAddr is
  // a node, and there is an edge between two squares if they touch.
  def getGraph: Map[QuadAddr, Set[QuadAddr]] = {
    // Create a list of all addresses
    var allAddresses = List[QuadAddr]()
    iter((addr, _) => allAddresses = addr :: allAddresses)

    // NOTE: This implementation is O(N^3) where N = # of addresses.
    var graph = Map[QuadAddr, Set[QuadAddr]]()
    for (a <- allAddresses) {
      var neighbors = Set[QuadAddr]()
      for (b <- allAddresses)
        if (a != b && (a touches b)) {
          neighbors += b
        }
      graph += ((a, neighbors))
    }
    graph
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
// NOTE: QuadBranch structure:
//
// +y
// ^^
// [ a b ] >
// [ c d ] > +x
//
// Outside of the qt package, it is preferred to use getSubtree() instead of
// referencing a/b/c/d directly, but both are allowed.
class QuadBranch[+T](val a: QuadTree[T],
    val b: QuadTree[T],
    val c: QuadTree[T],
    val d: QuadTree[T]) extends QuadTree[T] {

  val subtrees = List(a, b, c, d)

  def getSubtree(quadrant: Quadrant): QuadTree[T] = quadrant match {
    case TopLeft => a
    case TopRight => b
    case BottomLeft => c
    case BottomRight => d
  }

  // If this is a QuadBranch with 4 identical QuadLeafs, merge into one QuadLeaf
  // Else, return this
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

class QuadLeaf[+T](val data: T) extends QuadTree[T] {
  override def hashCode: Int = data.hashCode

  override def equals(o: Any): Boolean = o match {
      case other: QuadLeaf[T] => data == other.data
      case _ => false
  }
}

