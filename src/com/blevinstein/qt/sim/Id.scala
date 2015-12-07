package com.blevinstein.qt.sim

object Id {
  private var nextId: Int = 0

  // Returns nextId++
  def get: Id = {
    nextId += 1
    Id(nextId - 1)
  }

}
case class Id(val value: Int) {
  override def toString: String = s"Id($value)"
}
