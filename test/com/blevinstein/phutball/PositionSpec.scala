package com.blevinstein.phutball

import org.scalatest._

class PositionSpec extends FlatSpec with Matchers {
  "A Position" should "be valid when in bounds" in {
    new Position(0, 0).valid should be (true)
    Position.center.valid should be (true)

    new Position(15, 19).valid should be (false)
  }
}
