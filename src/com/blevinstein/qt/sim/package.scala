package com.blevinstein.qt

package object sim {
  type Id = Int
  type WorldIterCallback = (Id, QuadRectangle, Material) => Unit
}
