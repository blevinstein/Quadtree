package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.QuadOffset

class PhysicsModule extends WorldModule {
  val moveResolution = -6

  var gravity = Point.zero

  def getEvents(world: World) = {
    world.objs.
      flatMap { case (id, obj) =>
        obj.state match {
          case Fixed => List()
          case Moving(v) => List(
              MoveBy(id, QuadOffset.approx(v, moveResolution)),
              Accel(id, gravity))
        }
      }
  }
}
