package com.blevinstein.qt

package object sim {
  type ToolOutput = (Iterable[Drawable], Iterable[Event])
  type WorldIterCallback = (Id, QuadRectangle, Material) => Unit
}
