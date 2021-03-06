package com.blevinstein.qt.sim

import com.blevinstein.geom.Rectangle

import java.awt.Color

abstract class Drawable
case class FillRect(color: Color, rect: Rectangle) extends Drawable
case class FillRegion(color: Color, rects: List[Rectangle]) extends Drawable
