package com.blevinstein.qt.sim

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.{QuadAddr,QuadTree,QuadLeaf,QuadRectangle,QuadOffset}
import com.blevinstein.qt.sim.Operators.{anyOp,collideOp}
import com.blevinstein.util.Find.findMap

trait WorldModule {
  def getEvents(world: World): Iterable[Event]
}

// Mutable class describing a group of [objs] in space.
//
// Each object is defined as a QuadTree[Option[T]], so that we have a concept of
// empty space for collision.
class World(val objs: Map[Id, QuadObject], val modules: List[WorldModule]) {
  def this() = this(Map(), List())

  val moveRes = -7

  def velToOffset(velocity: Point): QuadOffset =
      QuadOffset.approx(velocity, moveRes)

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

  // Try to find an object with a leaf node containing the given point
  def find(point: Point): Option[(Id, QuadRectangle, Material)] =
      findMap(objs, (tuple: (Id, QuadObject)) => tuple match { case (id, obj) =>
          // bounds check
          if (obj.position.toRectangle.contains(point)) {
            // geometry check
            val addr =
                obj.shape.getAddr(point withRespectTo obj.position.toRectangle)
            obj.shape.getData(addr) match {
              case Some(material: Material) =>
                  Some((id, addr.toQuadRectangle within obj.position, material))
              case None => None
            }
          } else {
            None
          }})

  // Try to find an object with a region containing the given point
  def findRegion(point: Point): Option[(Id, List[QuadRectangle], Material)] =
    findMap(objs, (tuple: (Id, QuadObject)) => tuple match { case (id, obj) =>
        // bounds check
        if (obj.position.toRectangle.contains(point)) {
          // geometry check
          obj.shape.getRegions.
              find { case (material, addrs) =>
                  addrs.exists((addr) =>
                      addr.toQuadRectangle.toRectangle.contains(
                          point.withRespectTo(obj.position.toRectangle)))
              } match {
                case Some((Some(material), addrs: List[QuadAddr])) => Some((
                    id,
                    addrs.map{_.toQuadRectangle within obj.position},
                    material))
                case _ => None
              }
        } else {
          None
        }})

  // Process all events recursively, handling emitted events
  def afterAllEvents(event: Event): World = afterEvent(event) match {
    case (newWorld, events) =>
        events.foldLeft(newWorld)((w: World, e: Event) => w.afterAllEvents(e))
  }

  // Returns (newWorld, secondaryEvents), where [newWorld] represents the new
  // state of the world after [event], and [secondaryEvents] is a collection of
  // (zero or more) events that occur as a result of the primary event [event].
  // E.g. a MoveTo/MoveBy event can cause a Collision event.
  def afterEvent(event: Event): (World, Iterable[Event]) = event match {
    case Add(newId: Id, newObj: QuadObject) =>
        if (collideWithAll(newObj).isEmpty) {
          (withObjs(objs + ((newId, newObj))), List())
        } else {
          (this, List(Failed(event)))
        }
    case AddShape(id: Id, newShape: QuadTree[Option[Material]]) =>
      tryReplace(id, getObj(id).addShape(newShape))
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
        def arrestMotion(id: Id, vel: Point): Point = {
          val obj = getObj(id)
          if (canReplace(id, obj.withOffset(velToOffset(vel.xComp)))) {
            vel.xComp
          } else if (canReplace(id, obj.withOffset(velToOffset(vel.yComp)))) {
            vel.yComp
          } else {
            Point.zero
          }
        }
        val objA = getObj(idA)
        val objB = getObj(idB)
        (objA.state, objB.state) match {
          case (Moving(velA), Fixed) =>
              (this, List(SetVelocity(idA, arrestMotion(idA, velA))))
          case (Fixed, Moving(velB)) =>
              (this, List(SetVelocity(idB, arrestMotion(idB, velB))))
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

  // Calls [getEvents] on each [module], and process all those events.
  // To add new behaviors, [install] a new [WorldModule], e.g. [PhysicsModule]
  // or [ReaperModule].
  def update: World =
      process(modules.flatMap((module) => module.getEvents(this)))

  // Process a list of events by by calling [afterAllEvents] on each input
  // event.
  def process(events: Iterable[Event]): World =
      events.foldLeft(this)((w: World, e: Event) => w.afterAllEvents(e))

  // Considers creating a world where object [id] is replaced by [newObject]. If
  // that world is not legal (e.g. if any two objects are overlapping), will
  // return (this, List[Collision]). If that world is legal, will return
  // (newWorld, List()).
  def tryReplace(id: Id, newObject: QuadObject): (World, Iterable[Event]) = {
    val collisions = collideWithAll(newObject, Set(id))
    if (collisions.isEmpty) {
      (withObjs(objs + ((id, newObject))), List())
    } else {
      (this, collisions.map((otherId) => Collision(id, otherId)))
    }
  }

  // Helper method for static contact checking with all other objects.
  def contactsWithAll(id: Id): List[(Id, QuadAddr, QuadAddr)] =
      contactsWithAll(getObj(id), Set(id))

  // Get all contacts with objects in the world. "Contact" includes touching
  // along an edge or at a corner (a valid position).
  def contactsWithAll(obj: QuadObject, exclude: Set[Id] = Set()):
      List[(Id, QuadAddr, QuadAddr)] = {
    var contacts = List[(Id, QuadAddr, QuadAddr)]()
    for ((id, otherObj) <- objs if !exclude.contains(id)) {
      for ((rect, otherRect) <- obj.contacts(otherObj)) {
        ((rect withRespectTo obj.position).toAddressList,
            (otherRect withRespectTo otherObj.position).toAddressList) match {
          case (List(addr), List(otherAddr)) =>
              (id, addr, otherAddr) :: contacts
          case _ => ???
        }
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
