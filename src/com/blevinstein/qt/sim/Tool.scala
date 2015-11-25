package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadRectangle}

import java.awt.event.MouseEvent

trait Tool {
  def apply(world: World, input: List[Input]): Iterable[QuadRectangle]
}

object DeleteTool extends Tool {
  def apply(world: World, input: List[Input]): Iterable[QuadRectangle] =
      input match {
    case List(MouseInput(point: Point, MouseInput.HOVER)) =>
        world.find(point) match {
          case Some((id, rect, mat)) => List(rect)
          case None => List()
        }
    case _ :: List(MouseInput(point: Point, MouseEvent.BUTTON1)) =>
        world.find(point) match {
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
