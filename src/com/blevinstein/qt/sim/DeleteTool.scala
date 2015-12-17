package com.blevinstein.qt.sim

import com.blevinstein.geom.Point

import java.awt.Color
import java.awt.event.MouseEvent

case class DeleteTool(prefix: List[Input]) extends Tool {
  def clear(world: World, input: List[Input]): Boolean = input match {
    case hover :: MouseInput(point: Point, MouseEvent.BUTTON1) :: tail
        if tail startsWith prefix => true
    case _ => false
  }

  def activate(world: World, input: List[Input]): ToolOutput = input match {
    case MouseInput(point: Point, MouseInput.HOVER) :: tail
        if tail startsWith prefix =>
            world.find(point) match {
              case Some((id, rect, mat)) =>
                  (List(FillRect(Color.YELLOW, rect.toRectangle)), List())
              case None => Tool.Noop
            }
    case hover :: MouseInput(point: Point, MouseEvent.BUTTON1) :: tail
        if tail startsWith prefix =>
            // Find some leaf node that the user clicked on
            world.find(point) match {
              case Some((id, rect, mat)) => {
                  val obj = world.getObj(id)
                  (List(), List(
                      // Reshape the containing object, removing the leaf node
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

