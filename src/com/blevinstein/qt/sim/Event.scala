package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadOffset,QuadTree}

abstract class Event
case class Add(newId: Id, obj: QuadObject) extends Event

// Deprecated, use [Merge] instead
case class AddShape(id: Id, newShape: QuadTree[Option[Material]]) extends Event

case class Merge(id: Id, other: QuadObject) extends Event
case class Move(id: Id, velocity: Point) extends Event
case class MoveTo(id: Id, newPosition: QuadOffset) extends Event
case class Remove(id: Id) extends Event
case class Reshape(id: Id, newShape: QuadTree[Option[Material]]) extends Event

// Physics events
case class SetFixed(id: Id) extends Event
case class SetVelocity(id: Id, newVelocity: Point) extends Event
case class Accel(id: Id, deltaVelocity: Point) extends Event

// Feedback/emitted events
case class Collision(idA: Id, idB: Id) extends Event
case class Failed(event: Event) extends Event

