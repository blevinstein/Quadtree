package com.blevinstein.qt.sim

import com.blevinstein.qt.QuadTree

/// Mutable class describing a region of fixed size.
///
/// Contains an [env] describing fixed geometry, and [objs] containing objects
/// which can move about the region.
///
/// TODO: Remove env, just use objs. Simplifies and generalizes code.
///
/// TODO: Consider using Array or Vector, for performance reasons
/// TODO: Write tests to assess speed of implementation
class World(val env: QuadTree[Option[Material]],
    val objs: List[QuadObject]) {
  def this(env: QuadTree[Option[Material]]) = this(env, List[QuadObject]())
  val view: QuadTree[Option[Material]] = {
    val addOp = QuadTree.merge((m1: Option[Material], m2: Option[Material]) =>
        m2 match {
          case Some(mat) => m2
          case None => m1
        }) _

    var viewTree = env
    for (obj <- objs) {
      viewTree = addOp(viewTree, obj.toQuadTree)
    }
    viewTree
  }

  def add(obj: QuadObject): World = new World(env, obj :: objs)

  def update(f: (World, QuadObject) => QuadObject): World =
      new World(env, objs.map((obj) => f(this, obj)))

  override def toString: String = s"World(env=$env, objs=$objs)"
}

