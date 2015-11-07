package com.blevinstein.util

import org.scalatest._

class DeciderTest extends FunSuite with Matchers {
  test("choose basics") {
    val options = List(1, 2, 3, 4)
    val chosen = (0 until 100) map ((_) => Decider.choose(options))
    options foreach { case option =>
      chosen.contains(option) shouldEqual true // at least once
    }
    chosen foreach { case choice =>
      options.contains(choice) shouldEqual true // make sure all are valid
    }
  }
}
