package com.blevinstein.util

import org.scalatest._

class SearchTest extends FunSuite with Matchers {
  test("floodfill") {
    // Search space: integers 1-10
    // Use floodfill to get even/odd numbers
    def neighbors(num: Int): List[Int] =
        for (n <- List(num-2, num+2) if 0 < n && n <= 10) yield n

    Search.floodfill(1, neighbors).toSet shouldEqual Set(1, 3, 5, 7, 9)
    Search.floodfill(8, neighbors).toSet shouldEqual Set(2, 4, 6, 8, 10)
  }
}
