package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadOffset,QuadTree}

import java.awt.Color
import java.awt.event.MouseEvent

case class CopyTool(prefix: List[Input]) extends Tool {
  def clear(world: World, input: List[Input]): Boolean = input match {
    case hover ::
        MouseInput(pointB: Point, MouseEvent.BUTTON1) ::
        MouseInput(pointA: Point, MouseEvent.BUTTON1) ::
        tail if tail startsWith prefix => true
    case _ => false
  }

  def activate(world: World, input: List[Input]): ToolOutput = input match {
    // When the user hovers over a region, highlight it.
    case MouseInput(hoverPoint: Point, MouseInput.HOVER) ::
        tail if tail startsWith prefix =>
            world.findRegion(hoverPoint) match {
              case Some((id, rects, mat)) =>
                  (List(FillRegion(Color.YELLOW, rects.map{_.toRectangle})),
                      List())
              case None => Tool.Noop
            }
    case MouseInput(hoverPoint: Point, MouseInput.HOVER) ::
        MouseInput(pointA: Point, MouseEvent.BUTTON1) ::
        tail if tail startsWith prefix =>
            world.findRegion(pointA) match {
              case Some((id, rects, mat)) => {
                val offset =
                    QuadOffset.approx(hoverPoint - pointA, world.moveRes)
                val newRegion = rects.map{_ + offset}
                (List(FillRegion(Color.GREEN, newRegion.map{_.toRectangle})),
                    List())
              }
              case None => Tool.Noop
            }
    case hover ::
        MouseInput(pointB: Point, MouseEvent.BUTTON1) ::
        MouseInput(pointA: Point, MouseEvent.BUTTON1) ::
        tail if tail startsWith prefix =>
            world.findRegion(pointA) match {
              case Some((id, rects, material)) => {
                val obj = world.getObj(id)
                val offset = QuadOffset.approx(pointB - pointA, world.moveRes)
                val newRegion = rects
                    .map((rect) => (rect + offset).withRespectTo(obj.position))
                (List(), List(
                    AddShape(
                        id,
                        new QuadTree.Builder[Option[Material]](None)
                            .addAllRects(newRegion, Some(material))
                            .build)))
              }
              case None => Tool.Noop
            }
    case _ => Tool.Noop
  }
}
