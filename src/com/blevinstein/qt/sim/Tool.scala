package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.QuadRectangle

trait Tool {
  def render(world: World, p: Point): Iterable[QuadRectangle]
  def activate(world: World, p: Point): Unit
}
