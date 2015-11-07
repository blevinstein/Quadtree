package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadTree,QuadRectangle,QuadAddr,QuadOffset}

// Immutable container object for holding information about an object.
//
// Contains a QuadTree describing the [shape] of the object, and a QuadRectangle
// describing its [position].
class QuadObject(val position: QuadRectangle,
    val shape: QuadTree[Option[Material]],
    val state: State = Moving(Point.zero)) {
  require(position.isPerfectSquare, s"not a square: $position")

  // Converts into a QuadTree within a particular [space].
  def toQuadTree(space: QuadRectangle): QuadTree[Option[Material]] = {
    val posWithin = position withRespectTo space
    shape.grow(posWithin.perfectLog.get, posWithin.min, None)
  }

  def withOffset(offset: QuadOffset): QuadObject =
      new QuadObject(position + offset, shape, state)

  def withPosition(newPosition: QuadOffset): QuadObject = new QuadObject(
      new QuadRectangle(position.size) + (newPosition - (position.size >> 1)),
      shape,
      state)
  def withPosition(newPosition: QuadRectangle): QuadObject =
      new QuadObject(newPosition, shape, state)

  def withShape(newShape: QuadTree[Option[Material]]): QuadObject =
      new QuadObject(position, newShape, state)

  def withState(newState: State): QuadObject =
      new QuadObject(position, shape, newState)

  def canSplit: Boolean =
      shape.getRegions.filter { case (mat, addrs) => !mat.isEmpty }.size > 1

  def split: Iterable[QuadObject] =
      shape.getRegions
          .filter { case (mat, addrs) => !mat.isEmpty }
          .map { case (mat, addrs) => new QuadObject(
              position,
              new QuadTree.Builder[Option[Material]](None)
                  .addAll(addrs, mat)
                  .build,
              state) }


  val center: QuadOffset = position.min + (position.size >> 1)

  // Returns a list of squares where [this] is touching [other]. This includes
  // overlapping areas, as well as areas that share an edge or corner.
  def contacts(other: QuadObject):
      List[(QuadRectangle, QuadRectangle)] = {
    if (!(this.position touches other.position)) {
      List()
    } else {
      var contactList = List[(QuadRectangle, QuadRectangle)]()
      for (zone <- QuadZone.around(position)) {
        val thisTree = this.toQuadTree(zone.toQuadRectangle)
        val otherTree = other.toQuadTree(zone.toQuadRectangle)
        thisTree.iter((a: QuadAddr, aMat: Option[Any]) => {
          if (!aMat.isEmpty) {
            otherTree.iter((b: QuadAddr, bMat: Option[Any]) => {
              if (!bMat.isEmpty && (a touches b)) {
                contactList = (a.toQuadRectangle + zone.min,
                    b.toQuadRectangle + zone.min) :: contactList
              }
            })
          }
        })
      }
      contactList
    }
  }
}

