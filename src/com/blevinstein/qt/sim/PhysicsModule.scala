package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.QuadOffset

class PhysicsModule extends WorldModule {
  var gravity = Point.zero

  def getEvents(world: World): Iterable[Event] = {
    world.objs.
      flatMap { case (id, obj) =>
        obj.state match {
          case Fixed => List()
          case Moving(v) => List(
              Accel(id, gravity),
              MoveBy(id, world.velToOffset(v)))
        }
      }
  }
}
