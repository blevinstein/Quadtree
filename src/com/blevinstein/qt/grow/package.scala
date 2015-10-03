package com.blevinstein.qt

package object grow {
  // NOTE: Could be trivially refactored to T => Option[T]
  type QuadTransform = Material => Option[Material]
}

