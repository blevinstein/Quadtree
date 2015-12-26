package com.blevinstein.qt.sim

import com.blevinstein.geom.{Point,Rectangle}

object Camera {
  def focus(center: Point, zoom: Float): Camera =
      focus(center, Point.unit * zoom)
  def focus(center: Point, size: Point): Camera =
      Camera(new Rectangle(center - size / 2, center + size / 2))
}
case class Camera(rect: Rectangle) {
  def refocus(p: Point): Camera = Camera.focus(p, rect.size)
  def zoomIn(amount: Float): Camera =
      Camera.focus(rect.center, rect.size / amount)
  def zoomOut(amount: Float): Camera =
      Camera.focus(rect.center, rect.size * amount)

  // Transforms points within [rect] into the unit rectangle.
  def get(p: Point): Point = (p - rect.min) / rect.size
  def get(r: Rectangle): Rectangle = new Rectangle(get(r.min), get(r.max))

  // Transforms points within the unit rectangle to [rect]
  def put(p: Point): Point = p * rect.size + rect.min
  def put(r: Rectangle): Rectangle = new Rectangle(put(r.min), put(r.max))
}
