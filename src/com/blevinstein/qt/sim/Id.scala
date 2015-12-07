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

  override def hashCode: Int = value.hashCode

  override def equals(o: Any) = o match {
    case other: Id => value == other.value
    case _ => false
  }
}
