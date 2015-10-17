package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadTree,QuadLeaf,QuadRectangle,QuadOffset}

import scala.collection.mutable.HashMap

// Mutable class describing a group of [objs] in space.
//
// Each object is defined as a QuadTree[Option[T]], so that we have a concept of
// empty space.
//
// TODO: Write tests to assess speed of implementation?
class World[T] {
  private val objs: HashMap[Id, QuadObject] = new HashMap

  private var nextId = 0
  // returns nextId++
  def getId: Id = {
    nextId += 1
    nextId - 1
  }

  def getObj(id: Id): QuadObject = {
    require(objs.contains(id), s"objs does not contain id: $id")
    objs.get(id).get
  }

  def allObjs: Iterable[QuadObject] = objs.values

  def view: QuadTree[Option[T]] = {
    val addOp = QuadTree.merge((m1: Option[T], m2: Option[T]) =>
        m2 match {
          case Some(mat) => m2
          case None => m1
        }) _

    var viewTree: QuadTree[Option[T]] = new QuadLeaf(None)
    for (obj <- allObjs) {
      viewTree = addOp(viewTree, obj.toQuadTree)
    }
    viewTree
  }

  // Modification functions

  def add(position: QuadRectangle,
      shape: QuadTree[Option[T]]): Id = {
    val objId = getId
    objs.put(objId, new QuadObject(position, shape))
    objId
  }

  def move(id: Id, offset: QuadOffset): Unit = {
    val obj = getObj(id)
    objs.put(id, new QuadObject(obj.position + offset, obj.shape))
  }

  // Returns true if the object moves
  def moveIfPossible(id: Id, offset: QuadOffset): Boolean = {
    val obj = getObj(id)
    val newObj = new QuadObject(obj.position + offset, obj.shape)

    var collision = false
    for (otherObj <- allObjs)
      if (otherObj != obj && collidesWith(otherObj, newObj))
        collision = true

    if (collision) {
      false
    } else {
      objs.put(id, newObj)
      true
    }
  }

  // Collision helpers

  val collideOp = QuadTree.merge((m1: Option[T], m2: Option[T]) =>
      (m1, m2) match {
        case (Some(_), Some(_)) => true
        case _ => false
      }) _
  val anyOp = QuadTree.reduce((bs: List[Boolean]) => {
        bs.exists((b) => b)
      }) _
  def collidesWith(a: QuadObject, b: QuadObject): Boolean =
      if (anyOp(collideOp(a.toQuadTree, b.toQuadTree))) {
        true
      } else {
        false
      }

  override def toString: String = s"World(objs=$objs)"

  // Immutable container object for holding information about an object.
  class QuadObject(val position: QuadRectangle,
      val shape: QuadTree[Option[T]]) {
    require(position.isPerfectSquare, s"not a square: $position")

    val toQuadTree: QuadTree[Option[T]] =
        shape.grow(position.perfectLog.get, position.min, None)
  }
}

