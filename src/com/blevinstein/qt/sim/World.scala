package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadTree,QuadLeaf}

// Immutable class describing a region of fixed size.
//
// Contains [objs] which can be moved around.
//
// TODO: Add index, some persistent way to reference a moving object
//
// TODO: Consider using Array or Vector, for performance reasons
// TODO: Write tests to assess speed of implementation
class World(val objs: List[QuadObject]) {
  def this(objs: QuadObject*) = this(objs.toList)

  val view: QuadTree[Option[Material]] = {
    val addOp = QuadTree.merge((m1: Option[Material], m2: Option[Material]) =>
        m2 match {
          case Some(mat) => m2
          case None => m1
        }) _

    var viewTree: QuadTree[Option[Material]] = new QuadLeaf(Material.Empty)
    for (obj <- objs) {
      viewTree = addOp(viewTree, obj.toQuadTree)
    }
    viewTree
  }

  def add(obj: QuadObject): World = new World(obj :: objs)

  def update(f: (World, QuadObject) => QuadObject): World = new World(
      objs map ((obj) => f(this, obj)))

  override def toString: String = s"World(objs=$objs)"
}

