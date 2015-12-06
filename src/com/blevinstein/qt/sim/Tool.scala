package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadRectangle}

import java.awt.Color
import java.awt.event.MouseEvent

object Tool {
  val Noop = (List(), List())
}
trait Tool {
  def activate(world: World, input: List[Input]):
      (Iterable[Drawable], Iterable[Event])

  // When [clear] returns true, [inputStack] is cleared.
  def clear(world: World, input: List[Input]): Boolean
}

// TODO: take prefix: List[Input] argument? e.g. match prefix :: MouseInput  :: _
object DeleteTool extends Tool {
  def clear(world: World, input: List[Input]): Boolean = input match {
    case MouseInput(point: Point, MouseEvent.BUTTON1) :: _ => true
    case _ => false
  }

  def activate(world: World, input: List[Input]) =
      input match {
    case MouseInput(point: Point, MouseInput.HOVER) :: _ =>
        world.find(point) match {
          case Some((id, rect, mat)) =>
              (List(FillRect(Color.YELLOW, rect.toRectangle)), List())
          case None => Tool.Noop
        }
    case MouseInput(point: Point, MouseEvent.BUTTON1) :: _ =>
        world.find(point) match {
          case Some((id, rect, mat)) => {
            val obj = world.getObj(id)
            (List(), List(
                Reshape(
                    id,
                    obj.shape.toBuilder
                        .addAll(rect.withRespectTo(obj.position), None)
                        .build)))
          }
          case None => Tool.Noop
        }
    case _ => Tool.Noop
  }
}
