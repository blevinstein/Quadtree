package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}
import com.blevinstein.util.BiMap

// Can be used to express simple geometrical transformations on QuadTrees.
//
// NOTE: these transformations must be self-similar at all scales, because they
//   are applied recursively. Slightly more complex transforms (e.g. "swirl",
//   rotate left at one level, rotate right at next level down) could be
//   implemented by applying a (Transform => Transform) to the transformation
//   before applying it to the next level.
//   e.g. (transform) => new Transform(transform.map.inverse)
object Transform {
  val rotateLeft = new Transform(TopLeft -> BottomLeft, TopRight -> TopLeft,
      BottomRight -> TopRight, BottomLeft -> BottomRight)
  val rotateRight = new Transform(TopLeft -> TopRight, TopRight -> BottomRight,
      BottomRight -> BottomLeft, BottomLeft -> TopLeft)
  val mirror = new Transform(TopLeft -> TopRight, TopRight -> TopLeft,
      BottomLeft -> BottomRight, BottomRight -> BottomLeft)
}
class Transform(map: BiMap[Quadrant, Quadrant]) {
  def this(tuples: (Quadrant, Quadrant)*) = this(new BiMap(tuples : _*))

  def apply[T](tree: QuadTree[T]): QuadTree[T] = tree match {
    case branch: QuadBranch[T] => QuadBranch.create((quadrant) =>
        this.apply(branch.getSubtree(map.inverse(quadrant))))
    case leaf: QuadLeaf[T] => leaf
  }
}
