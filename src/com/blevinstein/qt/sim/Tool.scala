package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadRectangle}

trait Tool {
  def activate(world: World, p: Point): Unit
  def render(world: World, p: Point): Iterable[QuadRectangle]
}

object DeleteTool extends Tool {
  private def find(world: World, point: Point):
      Option[(Id, QuadRectangle, Material)] = {
    for ((id, obj) <- world.objs) {
      // bounds check
      if (obj.position.toRectangle.contains(point)) {
        // geometry
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

  def activate(world: World, point: Point): Unit = {
    find(world, point) match {
      case Some((id, rect, mat)) => {
        val obj = world.getObj(id)
        world.reshape(id,
            obj.shape.toBuilder
                .addAll(rect.withRespectTo(obj.position).toAddressList, None)
                .build)
      }
      case None => ()
    }
  }

  def render(world: World, point: Point): Iterable[QuadRectangle] = {
    find(world, point) match {
      case Some((id, rect, mat)) => List(rect)
      case None => List()
    }
  }
}
