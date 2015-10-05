package com.blevinstein.qt.sim

import java.awt.Color

object Material {
  val Empty: Option[Material] = None
  val Gray = Some(new Material(Color.LIGHT_GRAY))
  val Blue = Some(new Material(Color.BLUE))
}
class Material(val color: Color)
