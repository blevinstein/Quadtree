package com.blevinstein.qt.sim

import java.awt.Color

// Simple immutable class for describing different materials in the sim.
//
// TODO: Add "stickiness" and/or other properties, to determine behavior of
//   figure on contact (e.g. arrest momentum).

trait HasDensity {
  def density: Float
}

object Material {
  val Empty: Option[Material] = None
  val Gray = Some(new Material(Color.LIGHT_GRAY, 1f))
  val Blue = Some(new Material(Color.BLUE, 1f))
}
class Material(val color: Color, val density: Float) extends HasDensity

