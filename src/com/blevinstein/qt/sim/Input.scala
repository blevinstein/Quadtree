package com.blevinstein.qt.sim

import com.blevinstein.geom.Point

abstract class Input

object MouseInput {
  val HOVER: Int = 0
}
case class MouseInput(point: Point, button: Int) extends Input

case class KeyInput(keyCode: Int) extends Input

