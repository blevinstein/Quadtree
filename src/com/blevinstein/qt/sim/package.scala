package com.blevinstein.qt

package object sim {
  type Id = Int
  type WorldIterCallback[T] = (Id, QuadRectangle, T) => Unit
}
