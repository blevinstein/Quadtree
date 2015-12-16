package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadAddr,QuadOffset,QuadRectangle,QuadTree}

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
                    Merge(
                        id,
                        new QuadObject(
                            obj.position,
                            new QuadTree.Builder[Option[Material]](None)
                                .addAllRects(newRegion, Some(material))
                                .build,
                            obj.state))))
              }
              case None => Tool.Noop
            }
    case _ => Tool.Noop
  }
}

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

// TODO: Enable GrowTool to grow beyond obj.position. Requires simultaneously
// enlarging position and changing shape of obj.
case class GrowTool(prefix: List[Input]) extends Tool {
  // NOTE: input is in a stack, so the last input is first in the list. Thus,
  // we have (MouseInput(pointB), MouseInput(pointA), prefix, ...) but the
  // input actually happens in the order
  // (..., prefix /* reversed */, MouseInput(pointA), MouseInput(pointB))

  def clear(world: World, input: List[Input]): Boolean = input match {
    case hover ::
        MouseInput(pointB, MouseEvent.BUTTON1) ::
        MouseInput(pointA, MouseEvent.BUTTON1) ::
        tail if tail startsWith prefix => true
    case _ => false
  }

  // Given a [source] rectangle, returns a list of all new QuadRectangles (of
  // equal size, adjacent to source) where growth is allowed
  def growthPossibilities(source: QuadRectangle) = List(
      source + source.size.xComp,
      source - source.size.xComp,
      source + source.size.yComp,
      source - source.size.yComp,
      source + source.size,
      source - source.size,
      source + source.size.xComp - source.size.yComp,
      source - source.size.xComp + source.size.yComp)

  def activate(world: World, input: List[Input]): ToolOutput = input match {
    // When user is hovering over pointA, highlight source square
    case MouseInput(pointA, MouseInput.HOVER) ::
        tail if tail startsWith prefix => {
            world.find(pointA) match {
                case Some((id, rect, material)) =>
                    (List(FillRect(Color.GREEN, rect.toRectangle)), List())
                case None => Tool.Noop
            }
        }
    // After user has clicked a source square, highlight potential growth
    // locations.
    case MouseInput(pointB, MouseInput.HOVER) ::
        MouseInput(pointA, MouseEvent.BUTTON1) ::
        tail if tail startsWith prefix => {
            // Find some [rect] filled with [material] at [pointA]
            world.find(pointA) match {
              case Some((id, rect, material)) =>
                  val obj = world.getObj(id)
                  (growthPossibilities(rect).map((newRect) =>
                      FillRect(
                          // Draw growth possibilities in yellow, green when the
                          // user hovers over them.
                          if (newRect.toRectangle.contains(pointB)) {
                            Color.BLUE
                          } else {
                            Color.GREEN
                          },
                          newRect.toRectangle)), List())
              case None => Tool.Noop
            }
        }
    // Once the user has designed a source and destination square, try to grow
    // new material.
    case hover ::
        MouseInput(pointB, MouseEvent.BUTTON1) ::
        MouseInput(pointA, MouseEvent.BUTTON1) ::
        tail if tail startsWith prefix => {
          // Find some [rect] filled with [material] at [pointA]
          world.find(pointA) match {
            case Some((id, rect, material)) =>
                val obj = world.getObj(id)
                // From there, this tool allows us to "grow" in any direction by
                // copying [material] into an adjacent [newRect]
                growthPossibilities(rect).
                    find((newRect) => newRect.toRectangle.contains(pointB))
                        match {
                  case Some(newRect: QuadRectangle) => {
                      (List(), List(
                          // Reshape the containing object, copying [material]
                          // into [newRect].
                          // NOTE: This will happily overrwrite existing nodes
                          // in the source object, although it will fail if this
                          // would cause a collision with another object.
                          Merge(
                              id,
                              new QuadObject(
                                  obj.position,
                                  new QuadTree.Builder[Option[Material]](None)
                                      .addAll(
                                          newRect.withRespectTo(obj.position),
                                          Some(material))
                                      .build,
                                  obj.state))))
                          /*
                          TODO: diagnose why this doesn't work
                          Reshape(
                              id,
                              obj.shape.toBuilder
                                  .addAll(
                                      newRect.withRespectTo(obj.position),
                                      Some(material))
                                  .build)))
                          */
                  }
                  case None => Tool.Noop // Invalid second location
                }
            case None => Tool.Noop // No rect at first location
          }
        }
    case _ => Tool.Noop
  }
}
