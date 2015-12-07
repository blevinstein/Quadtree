package com.blevinstein.util

import com.blevinstein.util.Find.findMap

import org.scalatest._

class FindTest extends FunSuite with Matchers {
  test("findMap") {
    findMap(
        List(-2, -1, 0, 1, 2),
        (i: Int) => if (i > 0) { Some(i) } else { None }) shouldEqual Some(1)
    findMap(
        List(-3, -2, -1, 0),
        (i: Int) => if (i > 0) { Some(i) } else { None }) shouldEqual None
  }
}
