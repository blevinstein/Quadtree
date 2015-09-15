package com.blevinstein.qt

import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}
import com.blevinstein.qt.grow.ReplacementRule
import com.blevinstein.util.BiMap

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

  def apply(tree: QuadTree): QuadTree = tree match {
    case branch: QuadBranch => QuadBranch.create((quadrant) =>
        apply(branch.getSubtree(map.inverse(quadrant))))
    case leaf: QuadLeaf => leaf
  }

  def apply(rule: ReplacementRule): ReplacementRule =
      new ReplacementRule(apply(rule.before), apply(rule.after))
}
