package com.blevinstein.qt.sim

import com.blevinstein.geom.Point
import com.blevinstein.qt.{QuadLeaf,QuadOffset,QuadRectangle}

import org.scalatest._

class WorldTest extends FunSuite with Matchers {
  test("find") {
    val blockId = Id.get
    val blockPos = new QuadRectangle(new QuadOffset(5, 5), new QuadOffset(6, 6))
    val world = new World().process(List(
        Add(blockId, new QuadObject(blockPos, new QuadLeaf(Material.Blue)))
    ))
    world.find(new Point(5.5f, 5.5f)) shouldEqual
        Some((blockId, blockPos, Material.Blue.get))
  }
}
