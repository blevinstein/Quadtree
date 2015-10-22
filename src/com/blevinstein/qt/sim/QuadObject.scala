package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadTree,QuadRectangle,QuadAddr}

// Immutable container object for holding information about an object.
class QuadObject[T](val position: QuadRectangle,
    val shape: QuadTree[Option[T]]) {
  require(position.isPerfectSquare, s"not a square: $position")

  val toQuadTree: QuadTree[Option[T]] =
      shape.grow(position.perfectLog.get, position.min, None)

  def contacts(other: QuadObject[T]): List[(QuadAddr, QuadAddr)] = {
    val thisTree = this.toQuadTree
    val otherTree = other.toQuadTree
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

