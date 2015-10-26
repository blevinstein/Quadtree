package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadTree,QuadRectangle,QuadAddr}

// Immutable container object for holding information about an object.
//
// Contains a QuadTree describing the [shape] of the object, and a QuadRectangle
// describing its [position].
class QuadObject[T](val position: QuadRectangle,
    val shape: QuadTree[Option[T]]) {
  require(position.isPerfectSquare, s"not a square: $position")

  // Converts into a QuadTree within a particular [space].
  def toQuadTree(space: QuadRectangle): QuadTree[Option[T]] = {
    val posWithin = position within space
    shape.grow(posWithin.perfectLog.get, posWithin.min, None)
  }

  val center: Point = position.toRectangle.center

  // Returns a list of squares where [this] is touching [other] within [space].
  def contacts(other: QuadObject[T], space: QuadRectangle):
      List[(QuadAddr, QuadAddr)] = {
    val thisTree = this.toQuadTree(space)
    val otherTree = other.toQuadTree(space)
    var contactList = List[(QuadAddr, QuadAddr)]()
    thisTree.iter((a: QuadAddr, aMat: Option[Any]) => {
      if (!aMat.isEmpty) {
        otherTree.iter((b: QuadAddr, bMat: Option[Any]) => {
          if (!bMat.isEmpty && (a touches b)) {
            contactList = (a, b) :: contactList
          }
        })
      }
    })
    contactList
  }
}

