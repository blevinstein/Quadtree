package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadRectangle}

import java.awt.event.MouseEvent

trait Tool {
  def apply(world: World, input: List[Input]): Iterable[QuadRectangle]
}
object Tool {
  def find(world: World, point: Point):
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
}

object DeleteTool extends Tool {
  def apply(world: World, input: List[Input]): Iterable[QuadRectangle] =
      input match {
    case List(MouseInput(point: Point, MouseInput.HOVER)) =>
        Tool.find(world, point) match {
          case Some((id, rect, mat)) => List(rect)
          case None => List()
        }
    case _ :: List(MouseInput(point: Point, MouseEvent.BUTTON1)) =>
        Tool.find(world, point) match {
          case Some((id, rect, mat)) => {
            val obj = world.getObj(id)
            world.reshape(id,
                obj.shape.toBuilder
                    .addAll(rect.withRespectTo(obj.position).toAddressList, None)
                    .build)
            List()
          }
          case None => List()
        }
    case _ => List()
  }
}
