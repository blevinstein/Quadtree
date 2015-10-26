package com.blevinstein.qt.sim

import java.awt.Color

// Simple immutable class for describing different materials in the sim.
//
// TODO: Add "stickiness" and/or other properties, to determine behavior of
//   figure on contact (e.g. arrest momentum).
object Material {
  val Empty: Option[Material] = None
  val Gray = Some(new Material(Color.LIGHT_GRAY))
  val Blue = Some(new Material(Color.BLUE))
}
class Material(val color: Color)

