package com.blevinstein.qt.sim

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.QuadOffset

class ReaperModule extends WorldModule {
  val inf = Int.MaxValue
  val ninf = Int.MinValue

  var boundingRectangle =
      new Rectangle(new Point(ninf, ninf), new Point(inf, inf))

  // Reaper function. By default, moves object back to origin.
  var reaper: (World, Id, QuadObject) => Iterable[Event] =
      (world: World, id: Id, obj: QuadObject) =>
          List(MoveTo(id, QuadOffset.half), SetVelocity(id, Point.zero))

  def getEvents(world: World): Iterable[Event] = {
    world.objs.
        filter { case (id, obj) =>
            !boundingRectangle.contains(obj.center.toPoint) }.
        flatMap { case (id, obj) => reaper(world, id, obj) }
  }
}
