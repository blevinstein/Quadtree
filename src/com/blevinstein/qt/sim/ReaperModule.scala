package com.blevinstein.qt.sim

import com.blevinstein.geom.{Point,Rectangle}
import com.blevinstein.qt.QuadOffset

class ReaperModule extends WorldModule {
  val inf = Int.MaxValue
  val ninf = Int.MinValue

  var boundingRectangle =
      new Rectangle(new Point(ninf, ninf), new Point(inf, inf))

  // Reaper function. By default, moves object back to origin.
  var reaper = (world: World, id: Id, obj: QuadObject) => {
    world.moveTo(id, QuadOffset.half)
    world.setVelocity(id, Point.zero)
  }

  def update(world: World) {
    for ((id, obj) <- world.objs
         if !boundingRectangle.contains(obj.center.toPoint)) {
      reaper(world, id, obj)
    }
  }
}
