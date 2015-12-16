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
    val state: State = Moving(Point.zero)) {
  require(position.isPerfectSquare, s"not a square: $position")

  def combine(other: QuadObject): QuadObject = {
      val newPosition = subsume(position, other.position)
      println(s"combine $position + ${other.position} => $newPosition")

      val newState = (state, other.state) match {
        // TODO: preserve momentum not total velocity
        case (Moving(v1), Moving(v2)) => Moving(v1 + v2)
        case (Fixed, _) | (_, Fixed) => Fixed
        case _ => ???
      }

      new QuadObject(
          newPosition,
          addOp(this.toQuadTree(newPosition), other.toQuadTree(newPosition)),
          newState)
  }

  // Returns a QuadRectangle that contains both [a] and [b].
  def subsume(a: QuadRectangle, b: QuadRectangle): QuadRectangle = {
    // Choose [currentRect], and grow until it contains [otherRect]
    var currentRect = a
    val otherRect = b

    def grow(rect: QuadRectangle, posX: Boolean, posY: Boolean) =
        // Double the size of [rect], then shift in the -x or -y direction if
        // necessary.
        rect.resize(rect.size << 1) +
            (if (posX) { QuadOffset.zero } else { -rect.size.xComp }) +
            (if (posY) { QuadOffset.zero } else { -rect.size.yComp })

    while (!currentRect.contains(otherRect)) {
        currentRect = grow(
            currentRect,
            otherRect.min.x >= currentRect.min.x,
            otherRect.min.y >= currentRect.min.y)
    }

    currentRect
  } ensuring((result) => result.contains(a) && result.contains(b))

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
  def contacts(other: QuadObject): List[(QuadRectangle, QuadRectangle)] = {
    if (!(this.position touches other.position)) {
      List()
    } else {
      var contactList = List[(QuadRectangle, QuadRectangle)]()
      for (zone <- QuadZone.around(position)) {
        val thisTree = this.toQuadTree(zone.toQuadRectangle)
        val otherTree = other.toQuadTree(zone.toQuadRectangle)
        thisTree.iter((thisAddr: QuadAddr, thisMat: Option[Any]) => {
          if (!thisMat.isEmpty) {
            otherTree.iter((otherAddr: QuadAddr, otherMat: Option[Any]) => {
              if (!otherMat.isEmpty && (thisAddr touches otherAddr)) {
                contactList = (thisAddr.toQuadRectangle + zone.min,
                    otherAddr.toQuadRectangle + zone.min) :: contactList
              }
            })
          }
        })
      }
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

