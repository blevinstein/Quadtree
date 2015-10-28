package com.blevinstein.qt.sim

import com.blevinstein.geom.Point

abstract class State
case object Fixed extends State
case class Moving(velocity: Point) extends State

