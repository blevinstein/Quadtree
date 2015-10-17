package com.blevinstein.qt.sim

import com.blevinstein.qt.{QuadTree,QuadOffset,QuadRectangle}

// Events are both the inputs and the outputs of the World.
//
// Game logic results in a List[Event]. This list is sent to the World, which
// attempts to update the world in accordance with those events, and returns a
// new List[Event].
//
// TODO: Consider implementing Error(event: Event, reason: ?) extends Event
//   - id not found
sealed trait Event
// Input: Create(pos, shape)
// Output: Created(pos, shape, id)
case class Create(pos: QuadRectangle,
    shape: QuadTree[Option[Material]]) extends Event
case class Created(pos: QuadRectangle,
    shape: QuadTree[Option[Material]],
    id: Id) extends Event
// Input: Move(id, offset)
// Output: Move(id, offset) OR Collide(id, otherId, offset)
case class Move(id: Int, offset: QuadOffset) extends Event
case class Collide(mover: Int, blocker: Int, offset: QuadOffset) extends Event
// Input: Reshape(id, newShape)
// Output: Reshape(id, newShape) OR ReshapeBlocked(id, otherId, blockedArea)
case class Reshape(id: Int, newShape: QuadTree[Option[Material]]) extends Event
case class ReshapeBlocked
    (id: Int, otherId: Int, blockedArea: QuadTree[Boolean]) extends Event
// Input: Destroy(id)
// Output: Destroy(id)
case class Destroy(id: Int) extends Event

