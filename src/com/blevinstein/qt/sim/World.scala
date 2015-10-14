package com.blevinstein.qt.sim

import com.blevinstein.qt.QuadTree

object World {
  def from(tree: QuadTree[Option[Material]]): World =
    new World(tree, List())
}
class World(val env: QuadTree[Option[Material]],
    val objs: List[QuadObject]) {
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

  def withObjects(objs: List[QuadObject]): World = new World(env, objs)

  def update(f: (World, QuadObject) => QuadObject): World =
      new World(env, objs.map((obj) => f(this, obj)))

  override def toString: String = s"World(env=$env, objs=$objs)"
}

