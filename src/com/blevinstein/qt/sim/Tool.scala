package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadRectangle}

import java.awt.Color
import java.awt.event.MouseEvent

trait Tool {
  def activate(world: World, input: List[Input]): Iterable[Drawable]
  def clear(world: World, input: List[Input]): Boolean
  def trigger(world: World, input: List[Input]): Boolean
}

// TODO: take prefix: List[Input] argument? e.g. match prefix :: MouseInput  :: _
object DeleteTool extends Tool {
  def trigger(world: World, input: List[Input]): Boolean = input match {
    case MouseInput(point: Point, MouseInput.HOVER) :: _ => true
    case MouseInput(point: Point, MouseEvent.BUTTON1) :: _ => true
    case _ => false
  }

  def clear(world: World, input: List[Input]): Boolean = input match {
    case MouseInput(point: Point, MouseEvent.BUTTON1) :: _ => true
    case _ => false
  }

  def activate(world: World, input: List[Input]) =
      input match {
    case MouseInput(point: Point, MouseInput.HOVER) :: _ =>
        world.find(point) match {
          case Some((id, rect, mat)) =>
              List(FillRect(Color.YELLOW, rect.toRectangle))
          case None => List()
        }
    case MouseInput(point: Point, MouseEvent.BUTTON1) :: _ =>
        world.find(point) match {
          case Some((id, rect, mat)) => {
            val obj = world.getObj(id)
            world.reshape(id,
                obj.shape.toBuilder
                    .addAll(rect.withRespectTo(obj.position), None)
                    .build)
            List()
          }
          case None => List()
        }
    case _ => ???
  }
}
