package com.blevinstein.qt.grow

import com.blevinstein.qt.QuadAddr
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}
import com.blevinstein.util.RateLimiter

object GrowthSim {
  val environmentSize = 5 // environment side length = (2^environmentSize)
  val startAddress =
      new QuadAddr(BottomLeft ::
        List.tabulate(environmentSize - 1)((_) => TopRight))

  /*
   * Given a genome, grows a QuadTree
   * Returns (resulting QuadTree, number of steps to grow)
   */
  def apply(genome: QuadGenome): (QuadTree[Boolean], Int) = {
    var currentTree = new QuadTree.Builder[Boolean](false)
        .add(startAddress, true).build
    var steps = 0
    var done = false
    val maxSteps = 100
    val maxMillis = 100
    val maxGrowth = new RateLimiter(maxMillis)
    while (!done && steps < maxSteps && !maxGrowth.check) {
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
