package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadTree,QuadLeaf,QuadRectangle,QuadOffset}

import scala.collection.mutable.HashMap

// Mutable class describing a group of [objs] in space.
//
// Each object is defined as a QuadTree[Option[T]], so that we have a concept of
// empty space for collision.
class World {
  val moveResolution = -6

  private val objs: HashMap[Id, QuadObject] = new HashMap

  private var nextId = 0
  // Returns nextId++
  def getId: Id = {
    nextId += 1
    nextId - 1
  }

  def getObj(id: Id): QuadObject = {
    require(objs.contains(id), s"objs does not contain id: $id")
    objs.get(id).get
  }

  def allObjs: Iterable[QuadObject] = objs.values

  // physics
  var gravity = Point.zero

  def iter(cb: WorldIterCallback): Unit = {
    for ((objId, obj) <- objs) {
      obj.shape.iter((addr: QuadAddr, mat: Option[Material]) => {
        if (!mat.isEmpty) {
          cb(objId, addr.toQuadRectangle.within(obj.position), mat.get)
        }
      })
    }
  }

  def update: Unit = {
    for ((id, obj) <- objs) obj.state match {
      case Fixed => Unit
      case Moving(v) => {
        if (moveBy(id, QuadOffset.approx(v, moveResolution))) {
          accel(id, gravity)
        } else {
          // TODO: instead of just halving velocity, check whether colliding
          // with Fixed or Moving objects, try to update velocities of Moving
          // objects to synchronize
          objs.put(id, obj.withState(Moving(v / 2)))
        }
      }
    }
  }

  // Modification functions

  // Returns the Id of the added object, or None
  def add(newObj: QuadObject): Option[Id] = {
    val collision = !collideWithAll(newObj).isEmpty

    if (collision) {
      None
    } else {
      val objId = getId
      objs.put(objId, newObj)
      Some(objId)
    }
  }

  // Returns true if the object moves
  def moveTo(id: Id, position: QuadOffset): Boolean =
      tryReplace(id, getObj(id).withPosition(position))

  // Returns true if the object moves
  def moveBy(id: Id, offset: QuadOffset): Boolean =
      tryReplace(id, getObj(id).withOffset(offset))

  // Returns true if the object is be resized
  def reshape(id: Id, newShape: QuadTree[Option[Material]]): Boolean =
      tryReplace(id, getObj(id).withShape(newShape))

  // Returns true if the object is replaced
  def tryReplace(id: Id, newObject: QuadObject): Boolean = {
    if (collideWithAll(newObject, Set(id)).isEmpty) {
      objs.put(id, newObject)
      true
    } else {
      false
    }
  }

  def accel(id: Id, deltaVelocity: Point): Unit = {
    val obj = getObj(id)
    require(obj.state != Fixed, "Can't change velocity of a Fixed object.")
    obj.state match {
      case Moving(v) => objs.put(id, obj.withState(Moving(v + deltaVelocity)))
    }
  }

  def setVelocity(id: Id, newVelocity: Point): Unit = {
    val obj = getObj(id)
    require(obj.state != Fixed, "Can't change velocity of a Fixed object.")
    objs.put(id, obj.withState(Moving(newVelocity)))
  }

  def destroy(id: Id): Unit = {
    objs.remove(id)
  }

  def getMass(obj: QuadObject): Float = {
    val avgOp = QuadTree.reduce((xs: List[Float]) => xs.sum / xs.length) _
    val densityOp = QuadTree.transform((m: Option[Material]) => m match {
      case None => 0f
      case Some(m) => m.density
    }) _

    val boundingArea = obj.position.toRectangle.area

    avgOp(densityOp(obj.shape)) * boundingArea
  }

  // Get all contacts with other objects
  def contactsWithAll(id: Id): List[(Id, QuadRectangle, QuadRectangle)]
      = contactsWithAll(getObj(id), Set(id))
  def contactsWithAll(obj: QuadObject, exclude: Set[Id] = Set()):
      List[(Id, QuadRectangle, QuadRectangle)] = {
    var contacts = List[(Id, QuadRectangle, QuadRectangle)]()
    for ((id, otherObj) <- objs if !exclude.contains(id)) {
      for ((rect, otherRect) <- obj.contacts(otherObj)) {
        contacts = (id, rect, otherRect) :: contacts
      }
    }
    contacts
  }

  // Collision helpers

  def collideWithAll(obj: QuadObject, exclude: Set[Id] = Set()): List[Id] = {
    var collisions = List[Id]()
    for ((id, otherObj) <- objs if !exclude.contains(id))
      if (!(obj.position intersect otherObj.position).isEmpty) {
        if (collidesWith(obj, otherObj)) {
          collisions = id :: collisions
        }
      }
    collisions
  }

  // TODO: move QuadTree operators into separate file
  val collideOp = QuadTree.merge((m1: Option[Any], m2: Option[Any]) =>
      (m1, m2) match {
        case (Some(_), Some(_)) => true
        case _ => false
      }) _
  val anyOp = QuadTree.reduce((bs: List[Boolean]) => {
        bs.exists((b) => b)
      }) _
  def collidesWith(a: QuadObject, b: QuadObject): Boolean =
      if (anyOp(collideOp(
          a.toQuadTree(a.position),
          b.toQuadTree(a.position)))) {
        true
      } else {
        false
      }

  override def toString: String = s"World(objs=$objs)"
}
