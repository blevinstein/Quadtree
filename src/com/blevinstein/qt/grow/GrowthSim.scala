package com.blevinstein.qt.grow

import com.blevinstein.qt.Material
import com.blevinstein.qt.QuadAddr
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}

object GrowthSim {
  val environmentSize = 10 // environment side length = (2^environmentSize)
  val startAddress =
      new QuadAddr(BottomLeft ::
        List.tabulate(environmentSize - 1)((_) => TopRight))

  /*
   * Given a genome, grows a QuadTree
   * Returns (resulting QuadTree, number of steps to grow)
   */
  def apply(genome: QuadGenome): (QuadTree, Int) = {
    var currentTree = new QuadTree.Builder()
        .add(startAddress, Material.Full).build
    var steps = 0
    var done = false
    while (!done) {
      val newTree = ReplacementRule.update(genome)(currentTree)
      if (newTree == currentTree) {
        done = true
      } else {
        steps += 1
        currentTree = newTree
      }
    }
    (currentTree, steps)
  }
}
