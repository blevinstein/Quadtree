package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadTree,QuadLeaf,QuadRectangle,QuadOffset}

import scala.collection.mutable.HashMap

// Mutable class describing a group of [objs] in space.
//
// Each object is defined as a QuadTree[Option[T]], so that we have a concept of
// empty space for collision.
class World[T] {
  val moveResolution = -6

  private val objs: HashMap[Id, QuadObject[T]] = new HashMap

  private var nextId = 0
  // Returns nextId++
  def getId: Id = {
    nextId += 1
    nextId - 1
  }

  def getObj(id: Id): QuadObject[T] = {
    require(objs.contains(id), s"objs does not contain id: $id")
    objs.get(id).get
  }

  def allObjs: Iterable[QuadObject[T]] = objs.values

  def iter(cb: WorldIterCallback[T]): Unit = {
    for ((objId, obj) <- objs) {
      obj.shape.iter((addr: QuadAddr, mat: Option[T]) => {
        if (!mat.isEmpty) {
          cb(objId, addr.toQuadRectangle.within(obj.position), mat.get)
        }
      })
    }
  }

  def update: Unit = {
    for ((id, obj) <- objs) obj.state match {
      case Fixed => Unit
      case Moving(Point.zero) => Unit
      case Moving(v) => {
        if (!move(id, QuadOffset.approx(v, moveResolution))) {
          objs.put(id, obj.withState(Moving(v / 2)))
        }
      }
    }
  }

  // Modification functions

  // Returns the Id of the added object, or None
  def add(newObj: QuadObject[T]): Option[Id] = {
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
  def move(id: Id, offset: QuadOffset): Boolean =
      tryReplace(id, getObj(id).moved(offset))

  // Returns true if the object is be resized
  def reshape(id: Id, newShape: QuadTree[Option[T]]): Boolean =
      tryReplace(id, getObj(id).withShape(newShape))

  // Returns true if the object is replaced
  def tryReplace(id: Id, newObject: QuadObject[T]): Boolean = {
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

  // Collision helpers

  def collideWithAll(obj: QuadObject[T], exclude: Set[Id] = Set()): List[Id] = {
    var collisions = List[Id]()
    for ((id, otherObj) <- objs if !exclude.contains(id))
      if (collidesWith(obj, otherObj)) {
        collisions = id :: collisions
      }
    collisions
  }

  // TODO: move QuadTree operators into separate file
  val collideOp = QuadTree.merge((m1: Option[T], m2: Option[T]) =>
      (m1, m2) match {
        case (Some(_), Some(_)) => true
        case _ => false
      }) _
  val anyOp = QuadTree.reduce((bs: List[Boolean]) => {
        bs.exists((b) => b)
      }) _
  def collidesWith(a: QuadObject[T], b: QuadObject[T]): Boolean =
      if (anyOp(collideOp(
          a.toQuadTree(a.position),
          b.toQuadTree(a.position)))) {
        true
      } else {
        false
      }

  override def toString: String = s"World(objs=$objs)"
}
