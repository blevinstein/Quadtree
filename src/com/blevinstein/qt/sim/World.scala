package com.blevinstein.qt.sim

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.{QuadAddr,QuadTree,QuadLeaf,QuadRectangle,QuadOffset}
import com.blevinstein.qt.sim.Operators.{anyOp,collideOp}

import scala.collection.mutable.HashMap

trait WorldModule {
  def update(world: World): Unit
}

// Mutable class describing a group of [objs] in space.
//
// Each object is defined as a QuadTree[Option[T]], so that we have a concept of
// empty space for collision.
class World {
  var modules: List[WorldModule] = List()

  def install(module: WorldModule): World = {
    modules = module :: modules
    this
  }

  val objs: HashMap[Id, QuadObject] = new HashMap

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

  val inf = Int.MaxValue
  val ninf = Int.MinValue

  // bounds
  var boundingRectangle =
      new Rectangle(new Point(ninf, ninf), new Point(inf, inf))
  // reaper
  var reaper = (world: World, id: Id, obj: QuadObject) => {
    world.moveTo(id, QuadOffset.half)
    world.setVelocity(id, Point.zero)
  }

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
    modules.foreach((module) => module.update(this))
    // Reaper
    // TODO: refactor into module
    for ((id, obj) <- objs if !boundingRectangle.contains(obj.center.toPoint)) {
      reaper(this, id, obj)
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
  // Provides the underlying implementation for [moveTo], [moveBy], [reshape]...
  def tryReplace(id: Id, newObject: QuadObject): Boolean = {
    if (collideWithAll(newObject, Set(id)).isEmpty) {
      objs.put(id, newObject)
      true
    } else {
      false
    }
  }

  // Physics: change Moving object's velocity by [deltaVelocity]
  def accel(id: Id, deltaVelocity: Point): Unit = {
    val obj = getObj(id)
    require(obj.state != Fixed, "Can't change velocity of a Fixed object.")
    obj.state match {
      case Moving(v) => objs.put(id, obj.withState(Moving(v + deltaVelocity)))
    }
  }

  // Physics: set Moving object's velocity to [newVelocity]
  def setVelocity(id: Id, newVelocity: Point): Unit = {
    val obj = getObj(id)
    require(obj.state != Fixed, "Can't change velocity of a Fixed object.")
    objs.put(id, obj.withState(Moving(newVelocity)))
  }

  def destroy(id: Id): Unit = {
    objs.remove(id)
  }

  // Helper method for static contact checking with all other objects.
  def contactsWithAll(id: Id): List[(Id, QuadRectangle, QuadRectangle)]
      = contactsWithAll(getObj(id), Set(id))
  // Get all contacts with objects in the world. "Contact" includes touching
  // along an edge or at a corner (a valid position).
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

  // Helper method for checking whether an object collides with any other
  // object (i.e. has an impermissible overlap).
  // [tryReplace] will fail if the new object would "collide" with any other
  // existing object.
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

  // Helper method for checking whether two objects collide
  def collidesWith(a: QuadObject, b: QuadObject): Boolean =
      if (anyOp(collideOp(
          a.toQuadTree(a.position),
          b.toQuadTree(a.position)))) {
        true
      } else {
        false
      }

  // overrides

  override def toString: String = s"World(objs=$objs)"
}
