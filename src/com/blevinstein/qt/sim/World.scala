package com.blevinstein.qt.sim

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.{QuadAddr,QuadTree,QuadLeaf,QuadRectangle,QuadOffset}
import com.blevinstein.qt.sim.Operators.{anyOp,collideOp}

trait WorldModule {
  def getEvents(world: World): Iterable[Event]
}

// Mutable class describing a group of [objs] in space.
//
// Each object is defined as a QuadTree[Option[T]], so that we have a concept of
// empty space for collision.
class World(val objs: Map[Id, QuadObject], val modules: List[WorldModule]) {
  def this() = this(Map(), List())

  def withObjs(newObjs: Map[Id, QuadObject]) =
      new World(newObjs, modules)

  def install(module: WorldModule): World =
      new World(objs, module :: modules)

  def getObj(id: Id): QuadObject = {
    require(objs.contains(id), s"objs does not contain id: $id")
    objs.get(id).get
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

  def find(point: Point): Option[(Id, QuadRectangle, Material)] = {
    for ((id, obj) <- objs) {
      // bounds check
      if (obj.position.toRectangle.contains(point)) {
        // geometry check
        val addr =
            obj.shape.getAddr(point withRespectTo obj.position.toRectangle)
        obj.shape.getData(addr) match {
          case Some(material: Material) =>
            return Some(
                (id, addr.toQuadRectangle within obj.position, material))
          case None => ()
        }
      }
    }
    return None
  }

  // Process all events recursively, handling emitted events
  def afterAllEvents(event: Event): World = afterEvent(event) match {
    case (newWorld, events) =>
        events.foldLeft(newWorld)((w: World, e: Event) => w.afterAllEvents(e))
  }

  def afterEvent(event: Event): (World, Iterable[Event]) = event match {
    case Add(newId: Id, newObj: QuadObject) =>
        if (collideWithAll(newObj).isEmpty) {
          (withObjs(objs + ((newId, newObj))), List())
        } else {
          (this, List(Failed(event)))
        }
    case MoveBy(id: Id, offset: QuadOffset) =>
        tryReplace(id, getObj(id).withOffset(offset))
    case MoveTo(id: Id, newPosition: QuadOffset) =>
        tryReplace(id, getObj(id).withPosition(newPosition))
    case Remove(id: Id) => (withObjs(objs - id), List())
    case Reshape(id: Id, newShape: QuadTree[Option[Material]]) =>
        tryReplace(id, getObj(id).withShape(newShape))
    // Physics events
    case SetVelocity(id: Id, newVelocity: Point) =>
        tryReplace(id, getObj(id).withState(Moving(newVelocity)))
    case SetFixed(id: Id) =>
        tryReplace(id, getObj(id).withState(Fixed))
    case Accel(id: Id, deltaVelocity: Point) => {
        val obj = getObj(id)
        obj.state match {
          case Moving(v) =>
              tryReplace(id, obj.withState(Moving(v + deltaVelocity)))
          case Fixed => ???
        }
      }
    // Feedback/emitted events
    // TODO: instead of setting velocity to zero on collision, try setting only
    // velocity.x or velocity.y to zero, to enable "sliding".
    case Collision(idA: Id, idB: Id) => {
        val objA = getObj(idA)
        val objB = getObj(idB)
        (objA.state, objB.state) match {
          case (Moving(velA), Fixed) =>
              (this, List(SetVelocity(idA, Point.zero)))
          case (Moving(velA), Moving(velB)) => {
              // TODO: calculate mass, preserve avg momentum not avg velocity
              val newVelocity = (velA + velB) / 2
              (this, List(
                  SetVelocity(idA, newVelocity),
                  SetVelocity(idB, newVelocity)))
            }
        }
      }
    case Failed(event: Event) => {
        println(s"Failed Event: $event")
        (this, List())
      }
  }

  def update: World =
      process(modules.flatMap((module) => module.getEvents(this)))

  def process(events: Iterable[Event]): World = {
    events.foldLeft(this)((w: World, e: Event) => w.afterAllEvents(e))
  }

  def tryReplace(id: Id, newObject: QuadObject): (World, Iterable[Event]) = {
    val collisions = collideWithAll(newObject, Set(id))
    if (collisions.isEmpty) {
      (withObjs(objs + ((id, newObject))), List())
    } else {
      (this, collisions.map((otherId) => Collision(id, otherId)))
    }
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

  def canReplace(id: Id, newObject: QuadObject): Boolean =
      collideWithAll(newObject, Set(id)).isEmpty

  // Helper method for checking whether an object collides with any other
  // object (i.e. has an impermissible overlap).
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
  def collidesWith(a: QuadObject, b: QuadObject): Boolean = {
    // Choose the smaller region to check for collision
    val checkRegion: QuadRectangle =
        if (a.position.toRectangle.area < b.position.toRectangle.area) {
          a.position
        } else {
          b.position
        }
    if (anyOp(collideOp(
        a.toQuadTree(checkRegion),
        b.toQuadTree(checkRegion)))) {
      true
    } else {
      false
    }
  }

  // overrides

  override def toString: String = s"World(objs=$objs)"
}
