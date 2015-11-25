package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.QuadOffset

class PhysicsModule extends WorldModule {
  val moveResolution = -6

  var gravity = Point.zero

  def update(world: World): Unit = {
    for ((id, obj) <- world.objs) obj.state match {
      case Fixed => Unit
      case Moving(v) => {
        if (world.moveBy(id, QuadOffset.approx(v, moveResolution))) {
          world.accel(id, gravity)
        } else {
          // TODO: instead of just halving velocity, check whether colliding
          // with Fixed or Moving objects, try to update velocities of Moving
          // objects to synchronize
          world.objs.put(id, obj.withState(Moving(v / 2)))
        }
      }
    }
  }
}
