package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadAddr,QuadTree,QuadLeaf,QuadRectangle,QuadOffset}

import scala.collection.mutable.HashMap

// Mutable class describing a group of [objs] in space.
//
// Each object is defined as a QuadTree[Option[T]], so that we have a concept of
// empty space for collision.
//
// TODO: Write tests to assess speed of implementation?
class World[T] {
  private val objs: HashMap[Id, QuadObject[T]] = new HashMap

  private var nextId = 0
  // returns nextId++
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
    for (objId <- objs.keys) {
      val obj = objs.get(objId).get
      obj.shape.iter((addr: QuadAddr, mat: Option[T]) => {
        if (!mat.isEmpty) {
          cb(objId, addr.toQuadRectangle.within(obj.position), mat.get)
        }
      })
    }
  }

  // Modification functions

  def add(position: QuadRectangle,
      shape: QuadTree[Option[T]]): Id = {
    val objId = getId
    objs.put(objId, new QuadObject(position, shape))
    objId
  }

  // Returns true if the object moves
  def move(id: Id, offset: QuadOffset): Boolean = {
    val oldObj = getObj(id)
    val newObj = new QuadObject(oldObj.position + offset, oldObj.shape)

    var collision = !collideWithAll(newObj, Set(id)).isEmpty

    if (collision) {
      false
    } else {
      objs.put(id, newObj)
      true
    }
  }

  // Collision helpers

  def collideWithAll(obj: QuadObject[T], exclude: Set[Id] = Set()): List[Id] = {
    var collisions = List[Id]()
    for ((id, otherObj) <- objs if !exclude.contains(id))
      if (collidesWith(obj, otherObj))
        collisions = id :: collisions
    collisions
  }

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
