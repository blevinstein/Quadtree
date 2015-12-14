package com.blevinstein.qt.sim

import java.awt.Color

// Simple immutable class for describing different materials in the sim.
//
// TODO: Add "stickiness" and/or other properties, to determine behavior of
//   figure on contact (e.g. arrest momentum).

object Material {
  val Empty: Option[Material] = None
  val Gray = Some(Material(Color.LIGHT_GRAY, 1f))
  val Blue = Some(Material(Color.BLUE, 1f))
}
case class Material(color: Color, density: Float)

