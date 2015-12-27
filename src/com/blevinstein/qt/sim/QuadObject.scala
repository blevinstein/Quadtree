package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadTree,QuadRectangle,QuadAddr,QuadOffset}
import com.blevinstein.qt.sim.Operators.{addOp,avgOp,complexityOp};

// Immutable container object for holding information about an object.
//
// Contains a QuadTree describing the [shape] of the object, and a QuadRectangle
// describing its [position].
class QuadObject(val position: QuadRectangle,
    val shape: QuadTree[Option[Material]],
    val state: State) {
  require(position.isPerfectSquare, s"not a square: $position")

  def combine(other: QuadObject): QuadObject = {
      val newPosition = QuadRectangle.subsume(position, other.position)

      val newState = (state, other.state) match {
        // TODO: preserve momentum not average velocity
        case (Moving(v1), Moving(v2)) => Moving((v1 + v2) / 2)
        case (Fixed, _) | (_, Fixed) | (Fixed, Fixed) => Fixed
        case _ => ???
      }

      new QuadObject(
          newPosition,
          addOp(this.toQuadTree(newPosition), other.toQuadTree(newPosition)),
          newState)
  }

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

  def addShape(mask: QuadTree[Option[Material]]): QuadObject =
      new QuadObject(position, addOp(shape, mask), state)

  def withState(newState: State): QuadObject =
      new QuadObject(position, shape, newState)

  // TODO: add impulse between regions in contact?
  // TODO: add tool for splitting?
  // TODO: add image input?
  // Returns true if this object consists of 2 or more continuous regions of
  // material.
  def canSplit: Boolean =
      shape.getRegions.filter { case (mat, addrs) => !mat.isEmpty }.size > 1

  // Splits [this] into multiple QuadObjects, 1 for each continuous region of
  // material.
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
  // NOTE: This function is O(N^2) using callbacks, could be improved.
  def contacts(other: QuadObject): List[(QuadRectangle, QuadRectangle)] = {
    if (!(this.position touches other.position)) {
      List()
    } else {
      var contactList = List[(QuadRectangle, QuadRectangle)]()
      val contactZone = ((position - position.min) << 1) +
          position.min - (position.size >> 1)
      val thisTree = this.toQuadTree(contactZone)
      val otherTree = other.toQuadTree(contactZone)
      thisTree.iter((thisAddr: QuadAddr, thisMat: Option[Any]) => {
        if (!thisMat.isEmpty) {
          otherTree.iter((otherAddr: QuadAddr, otherMat: Option[Any]) => {
            if (!otherMat.isEmpty && (thisAddr touches otherAddr)) {
              contactList = (thisAddr.toQuadRectangle within contactZone,
                  otherAddr.toQuadRectangle within contactZone) :: contactList
            }
          })
        }
      })
      contactList
    }
  }

  // TODO: refactor into Physics module
  val densityOp = QuadTree.transform((m: Option[Material]) => m match {
    case None => 0f
    case Some(m) => m.density
  }) _

  // Returns totalMass = avgDensity * area
  def getMass: Float = {
    avgOp(densityOp(shape)) * position.toRectangle.area
  }

  override def toString: String =
      s"QuadObject(pos=$position, state=$state, shape=$shape)"
}

