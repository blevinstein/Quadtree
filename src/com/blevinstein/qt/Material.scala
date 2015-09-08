package com.blevinstein.qt

object Material {
  val Empty = new Material
  val Full = new Material
}

class Material {
  override def toString = this match {
    case Material.Empty => " "
    case Material.Full => "X"
  }
}

