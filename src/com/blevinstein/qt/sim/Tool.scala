package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadRectangle}

import java.awt.Color
import java.awt.event.MouseEvent

object Tool {
  val Noop: ToolOutput = (List(), List())
}
trait Tool {
  // This method returns [Drawable]s to draw on the screen, e.g. a cursor
  // or text messages to the user, as well as [Event]s that change the state of
  // the world.
  def activate(world: World, input: List[Input]): ToolOutput

  // When [clear] returns true, [inputStack] is cleared.
  // TODO: refactor into activate()? It's annoying that the return type of
  //   [activate] is getting so long. If it gets any bigger than 3 parts, I
  //   should create a class ToolOutput.
  def clear(world: World, input: List[Input]): Boolean
}

case class DeleteTool(prefix: List[Input]) extends Tool {
  def clear(world: World, input: List[Input]): Boolean = input match {
    case MouseInput(point: Point, MouseEvent.BUTTON1) :: prefix :: _ => true
    case _ => false
  }

  def activate(world: World, input: List[Input]) =
      input match {
    case MouseInput(point: Point, MouseInput.HOVER) :: prefix :: _ =>
        world.find(point) match {
          case Some((id, rect, mat)) =>
              (List(FillRect(Color.YELLOW, rect.toRectangle)), List())
          case None => Tool.Noop
        }
    case MouseInput(point: Point, MouseEvent.BUTTON1) :: prefix :: _ =>
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
