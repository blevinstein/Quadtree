package com.blevinstein.qt.grow

object Material {
  val Empty = new Material
  val Full = new Material
}

class Material {
  override def toString: String = this match {
    case Material.Empty => "_"
    case Material.Full => "X"
  }
}

